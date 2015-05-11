//
// Copyright (C) 2015 Robert Crowther
//
// CSlack is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
//
// CSlack is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License along with CSlack. If not, see <http://www.gnu.org/licenses/>.
//

//Based on "Parsing Expression Grammar of C for Mouse 1.1 - 1.5" which is
//  Copyright (C) 2007, 2009, 2010 by Roman R Redziejowski (www.romanredz.se).

package languageConverter

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}

/** Parses 'c' code, via Parboiled
  *
  * Without a full compiler front-end, parsing 'c' code is difficult.
  * This code is based in a Mouse Grammar, by Roman R
  * Redziejowski. However, the original solves the ambiguity of
  * identifiers clashing with typedefed types
  * 
  * {{{
  * const temp_in_degrees x;
  * }}}
  * 
  * (is temp_in_degrees an identifier lacking a type, or a type), by
  * rigourously applying typedef gathering and checking.
  *
  * While this is an easy action in Parboiled, it is very difficult to
  * gather all the definitions needed to decide if an identifier has
  * been typedefed or not - in C these definitions may be in many
  * files, and often are. So this parser takes a different approach,
  * assuming the convention that identifiers are always last in the
  * list (this is not rigourous c).
  */
class CSlack
    extends Parser
{
  
  // Testing

  // Partial tree accessors
  def ConstantUnit = rule { Spacing ~ oneOrMore(Constant) ~ EOI }
  def StringLiteralUnit = rule { Spacing ~ oneOrMore(StringLiteral) ~ EOI }
  def IdentifierUnit = rule { Spacing ~ oneOrMore(Identifier) ~ EOI }
  def DeclarationUnit = rule { Spacing ~ oneOrMore(Declaration) ~ EOI }


  //-------------------------------------------------------------------------
  //  A.2.4  External definitions
  //-------------------------------------------------------------------------

  // zeroOrMore, surely?
  def TranslationUnit = rule { Spacing ~ zeroOrMore(ExternalDeclaration) ~ EOI}

  def ExternalDeclaration = rule { FunctionDefinition | Declaration }

  def FunctionDefinition = rule { DeclarationSpecifiers ~ Declarator ~ optional(DeclarationList) ~ CompoundStatement }

  def DeclarationList = rule { oneOrMore(Declaration) }


  //-------------------------------------------------------------------------
  //  A.2.2  Declarations
  //-------------------------------------------------------------------------

  def Declaration = rule { 
    DeclarationSpecifiers ~ optional(InitDeclaratorList )  ~ SEMICOLON
  }

  def DeclarationSpecifiers = rule {
    (
      DeclarationSpecifiersLong
        | DeclarationSpecifiersShort
    )
  }

  def DeclarationSpecifiersLong = rule {
    zeroOrMore(
      StorageClassSpecifier
        | TypeQualifier
        | FunctionSpecifier
    ) ~ TypedefName ~ zeroOrMore(
      StorageClassSpecifier
        | TypeQualifier
        | FunctionSpecifier
    )
  }

  def DeclarationSpecifiersShort = rule {
    oneOrMore(
      StorageClassSpecifier
        | TypeSpecifier
        | TypeQualifier
        | FunctionSpecifier
    )
  }

  def InitDeclaratorList = rule {
    InitDeclarator ~ zeroOrMore(COMMA ~ InitDeclarator)
  }

  def InitDeclarator = rule { Declarator ~ optional(EQU ~ Initializer) }

  def StorageClassSpecifier = rule {
    (
      TYPEDEF
        | EXTERN
        | STATIC
        | AUTO
        | REGISTER
        | ATTRIBUTE ~ LPAR ~ LPAR ~ zeroOrMore(!RPAR ~ ANY) ~ RPAR ~ RPAR
    )
  }

  def TypeSpecifier = rule {
    (
      VOID
        | CHAR
        | SHORT
        | INT
        | LONG
        | FLOAT
        | DOUBLE
        | SIGNED
        | UNSIGNED
        | BOOL
        | COMPLEX
        | StructOrUnionSpecifier
        | EnumSpecifier
    )
  }

  def StructOrUnionSpecifier = rule {
    StructOrUnion ~
    (
      optional(Identifier) ~ LWING ~ oneOrMore(StructDeclaration) ~ RWING
        | Identifier
    )
  }

  def StructOrUnion = rule { STRUCT | UNION}

  def StructDeclaration = rule {
    SpecifierQualifierList ~ StructDeclaratorList ~ SEMICOLON
  }

  def SpecifierQualifierList: Rule0 = rule {
    (
      ( zeroOrMore(TypeQualifier) ~ TypedefName ~ zeroOrMore(TypeQualifier) )
        | oneOrMore( TypeSpecifier | TypeQualifier )
    )
  }

  def StructDeclaratorList = rule { 
    StructDeclarator ~ zeroOrMore(COMMA ~ StructDeclarator)
  }

  def StructDeclarator = rule {
    (
      optional(Declarator) ~ COLON ~ ConstantExpression
        | Declarator
    )
  }

  def EnumSpecifier = rule {
    ENUM ~
    ( optional(Identifier) ~ LWING ~ EnumeratorList ~ optional(COMMA) ~ RWING
      | Identifier
    )
  }

  def EnumeratorList = rule { Enumerator ~ zeroOrMore(COMMA ~ Enumerator) }

  def Enumerator = rule { 
    EnumerationConstant ~ optional(EQU ~ ConstantExpression)
  }

  def TypeQualifier = rule {
    (
      CONST
        | RESTRICT
        | VOLATILE
        | DECLSPEC ~ LPAR ~ Identifier ~ RPAR
    )
  }

  def FunctionSpecifier = rule { INLINE | STDCALL}

  def Declarator : Rule0 = rule { optional(Pointer) ~ DirectDeclarator }

  def DirectDeclarator = rule {
    ( Identifier | LPAR ~ Declarator ~ RPAR) ~ zeroOrMore(
      LBRK ~ zeroOrMore(TypeQualifier) ~ optional(AssignmentExpression) ~ RBRK
        | LBRK ~ STATIC ~ zeroOrMore(TypeQualifier) ~ AssignmentExpression ~ RBRK
        | LBRK ~ oneOrMore(TypeQualifier) ~ STATIC ~ AssignmentExpression ~ RBRK
        | LBRK ~ zeroOrMore(TypeQualifier) ~ STAR ~ RBRK
        | LPAR ~ ParameterTypeList ~ RPAR
        | LPAR ~ optional(IdentifierList) ~ RPAR
    )
  }

  def Pointer = rule { oneOrMore( STAR ~ zeroOrMore(TypeQualifier) ) }

  def ParameterTypeList = rule { ParameterList ~ optional(COMMA ~ ELLIPSIS) }

  def ParameterList = rule {
    ParameterDeclaration ~ zeroOrMore(COMMA ~ ParameterDeclaration)
  }

  def ParameterDeclaration = rule {
    DeclarationSpecifiers ~ optional( Declarator | AbstractDeclarator)
  }

  def IdentifierList = rule { Identifier ~ zeroOrMore(COMMA ~ Identifier) }

  def TypeName = rule { SpecifierQualifierList ~ optional(AbstractDeclarator) }

  def AbstractDeclarator: Rule0 = rule {
    (
      optional(Pointer) ~ DirectAbstractDeclarator
        | Pointer
    )
  }

  def DirectAbstractDeclarator = rule {
    (
      LPAR ~ AbstractDeclarator ~ RPAR
        | LBRK ~ optional(AssignmentExpression | STAR) ~ RBRK
        | LPAR ~ optional(ParameterTypeList) ~ RPAR
    ) ~ zeroOrMore(
      LBRK ~ optional(AssignmentExpression | STAR) ~ RBRK
        | LPAR ~ optional(ParameterTypeList) ~ RPAR
    )
  }

  def TypedefName = rule { Identifier }

  def Initializer: Rule0 = rule {
    (
      AssignmentExpression
        | LWING ~ InitializerList ~ optional(COMMA) ~ RWING
    )
  }

  def InitializerList = rule {
    optional(Designation) ~ Initializer ~ zeroOrMore(COMMA ~ optional(Designation) ~ Initializer)
  }

  def Designation = rule { oneOrMore(Designator) ~ EQU}

  def Designator = rule {
    (
      LBRK ~ ConstantExpression ~ RBRK
        | DOT ~ Identifier
    )
  }


  //-------------------------------------------------------------------------
  //  A.2.3  Statements
  //-------------------------------------------------------------------------

  def Statement: Rule0 = rule {
    (
      LabeledStatement
        | CompoundStatement
        | ExpressionStatement
        | SelectionStatement
        | IterationStatement
        | JumpStatement
    )
  }

  def LabeledStatement = rule {
    (
      Identifier ~ COLON ~ Statement
        | CASE ~ ConstantExpression ~ COLON ~ Statement
        | DEFAULT ~ COLON ~ Statement
    )
  }

  def CompoundStatement = rule {
    LWING ~ zeroOrMore( Declaration | Statement ) ~ RWING
  }

  def ExpressionStatement = rule { optional(Expression) ~ SEMICOLON}

  def SelectionStatement = rule {
    (
      IF ~ LPAR ~ Expression ~ RPAR ~ Statement ~ optional(ELSE ~ Statement)
        | SWITCH ~ LPAR ~ Expression ~ RPAR ~ Statement
    )
  }

  def IterationStatement = rule {
    (
      WHILE ~ LPAR ~ Expression ~ RPAR ~ Statement
        | DO ~ Statement ~ WHILE ~ LPAR ~ Expression ~ RPAR ~ SEMICOLON
        | FOR ~ LPAR ~ optional(Expression) ~ SEMICOLON ~ optional(Expression) ~ SEMICOLON ~ optional(Expression) ~ RPAR ~ Statement
        | FOR ~ LPAR ~ Declaration ~ optional(Expression) ~ SEMICOLON ~ optional(Expression) ~ RPAR ~ Statement
    )
  }

  def JumpStatement = rule {
    (
      GOTO ~ Identifier ~ SEMICOLON
        | CONTINUE ~ SEMICOLON
        | BREAK ~ SEMICOLON
        | RETURN ~ optional(Expression) ~ SEMICOLON
    )
  }


  //-------------------------------------------------------------------------
  //  A.2.1  Expressions
  //-------------------------------------------------------------------------

  def PrimaryExpression = rule {
    (
      Identifier
        | Constant
        | StringLiteral
        | LPAR ~ Expression ~ RPAR
    )
  }

  def PostfixExpression = rule {
    (
      PrimaryExpression
        | LPAR ~ TypeName ~ RPAR ~ LWING ~ InitializerList ~ optional(COMMA) ~ RWING
    ) ~ zeroOrMore(
      LBRK ~ Expression ~ RBRK
        | LPAR ~ optional(ArgumentExpressionList) ~ RPAR
        | DOT ~ Identifier
        | PTR ~ Identifier
        | INC
        | DEC
    )
  }

  def ArgumentExpressionList = rule {
    AssignmentExpression ~ zeroOrMore(COMMA ~ AssignmentExpression)
  }

  def UnaryExpression : Rule0 = rule {
    (
      PostfixExpression
        | INC ~ UnaryExpression
        | DEC ~ UnaryExpression
        | UnaryOperator ~ CastExpression
        | SIZEOF ~ (UnaryExpression | LPAR ~ TypeName ~ RPAR )
    )
  }

  def UnaryOperator = rule {
    ( AND | STAR | PLUS | MINUS | TILDA| BANG )
  }

  def CastExpression = rule { zeroOrMore(LPAR ~ TypeName ~ RPAR) ~  UnaryExpression}

  def MultiplicativeExpression = rule { CastExpression  ~ zeroOrMore((STAR | DIV | MOD) ~ CastExpression) }

  def AdditiveExpression = rule { MultiplicativeExpression  ~ zeroOrMore((PLUS | MINUS) ~ MultiplicativeExpression) }

  def ShiftExpression = rule { AdditiveExpression  ~ zeroOrMore((LEFT | RIGHT) ~ AdditiveExpression) }

  def RelationalExpression = rule { ShiftExpression  ~ zeroOrMore((LE | GE | LT | GT) ~ ShiftExpression) }

  def EqualityExpression = rule { RelationalExpression  ~ zeroOrMore((EQUEQU | BANGEQU) ~ RelationalExpression) }

  def ANDExpression = rule { EqualityExpression ~ zeroOrMore(AND ~ EqualityExpression) }

  def ExclusiveORExpression = rule { ANDExpression ~ zeroOrMore(HAT ~ ANDExpression) }

  def InclusiveORExpression = rule { ExclusiveORExpression ~ zeroOrMore(OR ~ ExclusiveORExpression) }

  def LogicalANDExpression = rule { InclusiveORExpression ~ zeroOrMore(ANDAND ~ InclusiveORExpression) }

  def LogicalORExpression = rule { LogicalANDExpression ~ zeroOrMore(OROR ~ LogicalANDExpression) }

  def ConditionalExpression = rule { LogicalORExpression ~ zeroOrMore(QUERY ~ Expression ~ COLON ~ LogicalORExpression) }

  def AssignmentExpression: Rule0 = rule {
    (
      UnaryExpression ~ AssignmentOperator ~ AssignmentExpression
        | ConditionalExpression
    )
  }

  def AssignmentOperator = rule {
    (
      EQU
        | STAREQU
        | DIVEQU
        | MODEQU
        | PLUSEQU
        | MINUSEQU
        | LEFTEQU
        | RIGHTEQU
        | ANDEQU
        | HATEQU
        | OREQU
    )
  }

  def Expression = rule {
    AssignmentExpression ~ zeroOrMore(COMMA ~ AssignmentExpression)
  }

  def ConstantExpression = rule { ConditionalExpression }


  //-------------------------------------------------------------------------
  //  A.1.1  Lexical elements
  //  Tokens are: Keyword, Identifier, Constant, StringLiteral, Punctuator.
  //  Tokens are separated by Spacing.
  //-------------------------------------------------------------------------

  def Spacing =  rule(SuppressNode) {
    zeroOrMore(
      WhiteSpace
        | LongComment
        | LineComment
        | Pragma
    )
  }

  def WhiteSpace = rule { anyOf(" \n\r\t\u000B\u000C") } // 7.4.1.10

  def LongComment = rule { ("/*" ~ zeroOrMore(!("*/") ~ ANY) ~ "*/") }   // 6.4.9

  def LineComment = "//" ~ zeroOrMore(!anyOf("\r\n") ~ ANY) ~ ("\r\n" | "\r" | "\n" | EOI)       // 6.4.9

  def Pragma      = "#" ~ zeroOrMore(!"\n" ~ ANY)       // Treat pragma as comment


  //-------------------------------------------------------------------------
  //  A.1.2  Keywords
  //-------------------------------------------------------------------------

  final def AUTO      = ("auto" ~ !IdChar ~ Spacing)
  final def BREAK     = ("break" ~ !IdChar ~ Spacing)
  final def CASE      = ("case" ~ !IdChar ~ Spacing)
  final def CHAR      = ("char" ~ !IdChar ~ Spacing)
  final def CONST     = ("const" ~ !IdChar ~ Spacing)
  final def CONTINUE  = ("continue" ~ !IdChar ~ Spacing)
  final def DEFAULT   = ("default" ~ !IdChar ~ Spacing)
  final def DOUBLE    = ("double" ~ !IdChar ~ Spacing)
  final def DO        = ("do" ~ !IdChar ~ Spacing)
  final def ELSE      = ("else" ~ !IdChar ~ Spacing)
  final def ENUM      = ("enum" ~ !IdChar ~ Spacing)
  final def EXTERN    = ("extern" ~ !IdChar ~ Spacing)
  final def FLOAT     = ("float" ~ !IdChar ~ Spacing)
  final def FOR       = ("for" ~ !IdChar ~ Spacing)
  final def GOTO      = ("goto" ~ !IdChar ~ Spacing)
  final def IF        = ("if" ~ !IdChar ~ Spacing)
  final def INT       = ("int" ~ !IdChar ~ Spacing)
  final def INLINE    = ("inline" ~ !IdChar ~ Spacing)
  final def LONG      = ("long" ~ !IdChar ~ Spacing)
  final def REGISTER  = ("register" ~ !IdChar ~ Spacing)
  final def RESTRICT  = ("restrict" ~ !IdChar ~ Spacing)
  final def RETURN    = ("return" ~ !IdChar ~ Spacing)
  final def SHORT     = ("short" ~ !IdChar ~ Spacing)
  final def SIGNED    = ("signed" ~ !IdChar ~ Spacing)
  final def SIZEOF    = ("sizeof" ~ !IdChar ~ Spacing)
  final def STATIC    = ("static" ~ !IdChar ~ Spacing)
  final def STRUCT    = ("struct" ~ !IdChar ~ Spacing)
  final def SWITCH    = ("switch" ~ !IdChar ~ Spacing)
  final def TYPEDEF   = ("typedef" ~ !IdChar ~ Spacing)
  final def UNION     = ("union" ~ !IdChar ~ Spacing)
  final def UNSIGNED  = ("unsigned" ~ !IdChar ~ Spacing)
  final def VOID      = ("void" ~ !IdChar ~ Spacing)
  final def VOLATILE  = ("volatile" ~ !IdChar ~ Spacing)
  final def WHILE     = ("while" ~ !IdChar ~ Spacing)
  final def BOOL      = ("_Bool" ~ !IdChar ~ Spacing)
  final def COMPLEX   = ("_Complex" ~ !IdChar ~ Spacing)
  final def STDCALL   = ("_stdcall" ~ !IdChar ~ Spacing)
  final def DECLSPEC  = ("__declspec" ~ !IdChar ~ Spacing)
  final def ATTRIBUTE = ("__attribute__" ~ !IdChar ~ Spacing)

  def IsKeyword = rule(SuppressSubnodes) {
    (
      "auto"
        | "break"
        | "case"
        | "char"
        | "const"
        | "continue"
        | "default"
        | "double"
        | "do"
        | "else"
        | "enum"
        | "extern"
        | "float"
        | "for"
        | "goto"
        | "if"
        | "int"
        | "inline"
        | "long"
        | "register"
        | "restrict"
        | "return"
        | "short"
        | "signed"
        | "sizeof"
        | "static"
        | "struct"
        | "switch"
        | "typedef"
        | "union"
        | "unsigned"
        | "void"
        | "volatile"
        | "while"
        | "_Bool"
        | "_Complex"
        | "_Imaginary"
        | "_stdcall"
        | "__declspec"
        | "__attribute__"
    ) ~ !IdChar
  }


  //-------------------------------------------------------------------------
  //  A.1.3  Identifiers
  //  The standard does not explicitly state that identifiers must be
  //  distinct from keywords, but it seems so.
  //-------------------------------------------------------------------------

  def Identifier = rule(SuppressSubnodes) { !IsKeyword ~ IdNondigit ~ zeroOrMore(IdChar) ~ Spacing }

  def IdNondigit = rule { "a" - "z" | "A" - "Z" | "_" | UniversalCharacter }

  def IdChar = rule { "a"-"z" | "A"-"Z" | "0" - "9" | "_" | UniversalCharacter }


  //-------------------------------------------------------------------------
  //  A.1.4  Universal character names
  //-------------------------------------------------------------------------

  def UniversalCharacter = rule(SuppressSubnodes) {(
    "u" ~ HexQuad
      | "U" ~ HexQuad ~ HexQuad
  )}

  def HexQuad = rule { HexDigit ~ HexDigit ~ HexDigit ~ HexDigit }


  //-------------------------------------------------------------------------
  //  A.1.5  Constants
  //-------------------------------------------------------------------------

  def Constant = rule {
    (
      FloatConstant
        | IntegerConstant       // Note: can be a prefix of Float Constant!
        | EnumerationConstant
        | CharacterConstant
    )
  }

  def IntegerConstant = rule {
    (
      DecimalConstant
        | HexConstant
        | OctalConstant
    ) ~ optional(IntegerSuffix) ~ Spacing
  }
  
  def Sign = rule { anyOf("+-") }
  
  def DecimalDigit = rule { "0" - "9" }

  def DecimalConstant = rule(SuppressSubnodes) {
    ("1" - "9") ~ zeroOrMore(DecimalDigit)
  }

  def OctalConstant   = rule(SuppressSubnodes) { "0" ~ zeroOrMore(OctalDigit) }

  def HexConstant     = rule(SuppressSubnodes) { HexPrefix ~ oneOrMore(HexDigit) }

  def HexPrefix       = rule { "0x" | "0X" }

  def HexDigit  = rule { "a" - "f" | "A" - "F" | "0" - "9" }

  def IntegerSuffix = rule {
    (
      anyOf("uU") ~ optional(Lsuffix)
        | Lsuffix ~ optional(anyOf("uU"))
    )
  }

  def Lsuffix = rule { "ll" | "LL" | "l" | "L" }

  def FloatConstant =  rule {
    (
      (
        DecimalFloatConstant
          | HexFloatConstant
      ) ~ optional(FloatSuffix) ~ Spacing
    )
  }

  def DecimalFloatConstant =  rule(SuppressSubnodes) {
    (
      Fraction ~ optional( Exponent )
        |  oneOrMore(DecimalDigit) ~ Exponent
    )
  }

  def HexFloatConstant =  rule(SuppressSubnodes) {
    (
      HexPrefix ~ HexFraction ~ optional(BinaryExponent)
        | HexPrefix ~ oneOrMore(HexDigit) ~ BinaryExponent
    )
  }

  def Fraction =  rule {
    (
      "." ~ oneOrMore(DecimalDigit)
        | oneOrMore(DecimalDigit) ~ "." ~ optional(oneOrMore(DecimalDigit))
    )
  }

  def HexFraction = rule {
    (
      "." ~ oneOrMore(HexDigit)
        | oneOrMore(HexDigit) ~ "." ~ optional(oneOrMore(HexDigit))
    )
  }

  def Exponent = rule { anyOf("eE") ~ optional(Sign) ~ oneOrMore(DecimalDigit) }

  def BinaryExponent = rule {
    anyOf("pP") ~ optional(Sign) ~ oneOrMore(DecimalDigit)
  }

  def FloatSuffix = rule { anyOf("flFL") }

  def EnumerationConstant = rule { Identifier }
  
  // I suppose this must handle '\0' and the like...
  def CharacterConstant = rule { optional("L") ~ "'" ~ zeroOrMore(Char) ~ "'" ~ Spacing }

  def Char = rule { Escape | (!anyOf("'\n\\") ~ ANY) }

  def Escape = rule {
    "\\" ~ (SimpleEscape | OctalEscape | HexEscape | UniversalCharacter)
  }

  def SimpleEscape = rule(SuppressNode) { anyOf("'\"?\\abfnrtv") }
  
  def OctalDigit = rule { "0" - "7" }

  def OctalEscape = rule(SuppressSubnodes) {
    OctalDigit ~ optional(OctalDigit) ~ optional(OctalDigit)
  }

  def HexEscape = rule(SuppressSubnodes) { "x" ~ oneOrMore(HexDigit) }


  //-------------------------------------------------------------------------
  //  A.1.6  String Literals
  //-------------------------------------------------------------------------

  // Do not know whay multiple declarations allowed... R.C.
  def StringLiteral = rule {
    optional("L") ~ "\"" ~ zeroOrMore(StringChar) ~ "\"" ~ Spacing
  }

  def StringChar = rule { Escape | !anyOf("\"\n\\") ~ ANY }


  //-------------------------------------------------------------------------
  //  A.1.7  Punctuators
  //-------------------------------------------------------------------------

  final def LBRK       =  ("[" ~ Spacing)
  final def RBRK       =  ("]" ~ Spacing)
  final def LPAR       =  ("(" ~ Spacing)
  final def RPAR       =  (")" ~ Spacing)
  final def LWING      =  ("{" ~ Spacing)
  final def RWING      =  ("}" ~ Spacing)
  final def DOT        =  ("." ~ Spacing)
  final def PTR        =  ("->" ~ Spacing)
  final def INC        =  ("++" ~ Spacing)
  final def DEC        =  ("--" ~ Spacing)
  final def AND        =  ("&" ~ !"&" ~ Spacing)
  final def STAR       =  ("*" ~ !"=" ~ Spacing)
  final def PLUS       =  ("+" ~ !anyOf("+=") ~ Spacing)
  final def MINUS      =  ("-" ~ !anyOf("-=>") ~ Spacing)
  final def TILDA      =  ("~" ~ Spacing)
  final def BANG       =  ("!" ~ !"=" ~ Spacing)
  final def DIV        =  ("/" ~ !"=" ~ Spacing)
  final def MOD        =  ("%" ~ !anyOf("=>") ~ Spacing)
  final def LEFT       =  ("<<" ~ !"=" ~ Spacing)
  final def RIGHT      =  (">>" ~ !"=" ~ Spacing)
  final def LT         =  ("<" ~ !"=" ~ Spacing)
  final def GT         =  (">" ~ !"=" ~ Spacing)
  final def LE         =  ("<=" ~ Spacing)
  final def GE         =  (">=" ~ Spacing)
  final def EQUEQU     =  ("==" ~ Spacing)
  final def BANGEQU    =  ("!=" ~ Spacing)
  final def HAT        =  ("^" ~ !"=" ~ Spacing)
  final def OR         =  ("|" ~ !"=" ~ Spacing)
  final def ANDAND     =  ("&&" ~ Spacing)
  final def OROR       =  ("||" ~ Spacing)
  final def QUERY      =  ("?" ~ Spacing)
  final def COLON      =  (":" ~ !">" ~ Spacing)
  final def SEMICOLON  =  (";" ~ Spacing)
  final def ELLIPSIS   =  ("..." ~ Spacing)
  final def EQU        =  ("=" ~ !"=" ~ Spacing)
  final def STAREQU    =  ("*=" ~ Spacing)
  final def DIVEQU     =  ("/=" ~ Spacing)
  final def MODEQU     =  ("%=" ~ Spacing)
  final def PLUSEQU    =  ("+=" ~ Spacing)
  final def MINUSEQU   =  ("-=" ~ Spacing)
  final def LEFTEQU    =  ("<<=" ~ Spacing)
  final def RIGHTEQU   =  (">>=" ~ Spacing)
  final def ANDEQU     =  ("&=" ~ Spacing)
  final def HATEQU     =  ("^=" ~ Spacing)
  final def OREQU      =  ("|=" ~ Spacing)
  final def COMMA      =  ("," ~ Spacing)
  

  //-------------------------------------------------------------------------
  //  Parboiled helper methods
  //-------------------------------------------------------------------------

  def Keyword(kw: String) = rule {
    //Terminal(kw, IdChar)
    (kw ~ !IdChar ~ Spacing)
  }

  def Terminal(s: String) = rule {
    (s ~ Spacing).label("-" + s + "-")
  }

  def Terminal(s: String, mustNotFollow: Rule) = rule {
    (s ~ !mustNotFollow ~ Spacing).label("-" + s + "-")
  }

}//CSlack
