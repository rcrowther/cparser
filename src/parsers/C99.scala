/**
Copyright (C) 2015 Robert Crowther

C99 is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.

C99 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with C99.  If not, see <http://www.gnu.org/licenses/>.
*/

package languageConverter

import org.parboiled.scala._
import org.parboiled.errors.{ErrorUtils, ParsingException}


/** Parsing Expression Grammar for C as a parboiled parser.
  *
  *  Based on a web distributed BISON parser e.g.
  *
  * [[http://www.lysator.liu.se/c/ANSI-C-grammar-y.html]]
  *
  * AFAIK the BISON parser is based in appendicies to draft
  * specifications for the c language. Here is a later version,
  *  
  * [[http://www.open-std.org/jtc1/sc22/wg14/www/docs/n1124.pdf]]
  *
  * Niether reference document has copyright attached, but will have
  * copyright associated.
  *
  * I've renamed some items. There are notes in the (extensive)
  * documentation where I have done this. I have also taken advantage
  * of PEG parsing in places, and added a few later specification
  * items. Notably, the grammar now includes preprocessor rules, which
  * any non-theoretical parser will need.
  *
  */
class C99
    extends Parser
    with InParser
    with WhiteSpace
{

  //-------------------------------------------------------------------------
  //  Tokens
  //-------------------------------------------------------------------------

  // D
  def Digit = rule { "0" - "9" }
  // L
  def Letter = rule { "a" - "z" | "A" - "Z" | "_" }
  // H
  def Hex = rule { "0" - "9" | "a" - "f" | "A" - "Z" }


  //-------------------------------------------------------------------------
  // Numeric constants
  //-------------------------------------------------------------------------


  def Sign = rule { anyOf("+-") }

  /**
    * Draft C Spec: E/e
    */
  def Exponent = rule { ("E" | "e") ~ optional(Sign) ~ oneOrMore("0" - "9" | "a" - "f" | "A" - "Z") }

  /**
    * Draft C Spec: P/p
    */
  def BinaryExponent = rule { ("P" | "p") ~ optional(Sign)  ~ oneOrMore("0" - "9" ) }
  //FS
  def FloatSuffix = rule {  oneOrMore( anyOf("fFlL") ) }
  //IS
  //def IntegerSuffix = rule { zeroOrMore( anyOf("uUlL") )}


  /** Is a letter, digit, or usually significant identifier character.
    *
    * May be used to test an identifying word is complete, yet
    * allowing following operations, for example {{{counter++}}}
    * (testing for whitespace at the end of the word would fail, this
    * will not, yet show the word is syntacticly distinct).
    */
  def LetterOrDigit = rule(SuppressNode)  { "a" - "z" | "A" - "Z" | "0" - "9" | "_" | "$" }




  def BasedNumeral = rule(SuppressSubnodes)  { "0" ~ (
    // Hex
    (anyOf("xX") ~ oneOrMore(Hex) ~ optional(IntegerSuffix)) |
      // Octal
      (oneOrMore(Digit) ~ optional(IntegerSuffix))
  )}

  def StockNumeral = rule(SuppressSubnodes)  { oneOrMore(Digit) ~ (
    optional(IntegerSuffix) |
      // Whatever this is, not using...
      //(optional(Letter)'(\\.|[^\\'])+'
      (Exponent ~ optional(FloatSuffix)) |
      // Should be zeroOrMore, but...
      ("." ~ oneOrMore(Digit) ~ "(" ~ optional(Exponent) ~ ")" ~ optional(FloatSuffix))
  )}

  //def NumericLiteral = rule { (BasedNumeral | StockNumeral)  ~ Spacing }


  ///////////////////////////

  //-------------------------------------------------------------------------
  // 6.4.4 Constants
  //-------------------------------------------------------------------------
  def OctalDigit = rule { "0" - "7" }

  def HexadecimalDigit = rule { "0" - "9" | "a" - "f" | "A" - "F" }

  def HexadecimalDigitSequence = rule { oneOrMore(HexadecimalDigit) }

  def DigitSequence = rule { oneOrMore(Digit) }

  def FloatingSuffix = rule {
    anyOf("flFL")
  }

  def BinaryExponentPart = rule {
    anyOf("pP") ~ optional(Sign) ~ DigitSequence
  }


  //-------------------------------------------------------------------------
  //  String Literals
  //-------------------------------------------------------------------------
  def Escape = rule{ "\\" ~ ANY }




  def CharLiteral = rule { optional("L") ~ "'" ~ ( Escape | (!anyOf("'\\") ~ ANY)) ~ "'" ~ Spacing }


  /*
   def CharacterConstant  = rule {
   ' c-char-sequence '
   L' c-char-sequence '
   }
   */

  //-------------------------------------------------------------------------
  //  Identifiers
  //-------------------------------------------------------------------------
  def Identifier = rule(SuppressSubnodes) { Letter ~ zeroOrMore(Letter|Digit) ~ Spacing }

  def IdentifierNoSpace = rule { Letter ~ zeroOrMore(Letter|Digit) }




  //-------------------------------------------------------------------------
  //  helper methods
  //-------------------------------------------------------------------------

  def Keyword(keyword: String) = rule {
    Terminal(keyword, LetterOrDigit)
  }

  def Terminal(string: String) = rule {
    (string ~ Spacing).label("-" + string + "-")
  }

  def Terminal(string: String, mustNotFollow: Rule) = rule {
    (string ~ !mustNotFollow ~ Spacing).label("-" + string + "-")
  }



  //-------------------------------------------------------------------------
  // Separators, Operators
  //-------------------------------------------------------------------------

  // N.B. These are not working with the Terminal declaration
  // from Java, but not in Scala
  final def ASSIGN = rule {
    ("=" ~ !"=" ~ Spacing).label("-" + "=" + "-")
  }

  final def PLUS = rule {
    ("+" ~ !anyOf("=+") ~ Spacing).label("-" + "+" + "-")
  }

  final def MINUS = rule {
    ("-" ~ !anyOf("=->") ~ Spacing).label("-" + "-" + "-")
  }

  final def DIV = rule {
    ("/" ~ !"=" ~ Spacing).label("-" + "/" + "-")
  }

  final def STAR = rule {
    ("*" ~ !"=" ~ Spacing).label("-" + "*" + "-")
  }

  final def MOD = rule {
    ("%" ~ !"=" ~ Spacing).label("-" + "%" + "-")
  }

  final def AND = rule {
    ("&" ~ !anyOf("=&") ~ Spacing).label("-" + "&" + "-")
  }

  final def OR = rule {
    ("|" ~ !anyOf("=|") ~ Spacing).label("-" + "|" + "-")
  }

  final def XOR = rule {
    ("^" ~ !"=" ~ Spacing).label("-" + "^" + "-")
  }

  final def NOT = rule {
    ("!" ~ !"=" ~ Spacing).label("-" + "!" + "-")
  }

  final def TITHE = rule {
    ("~" ~ Spacing).label("-" + "~" + "-")
  }

  final def GT = rule {
    (">" ~ !anyOf("=>") ~ Spacing).label("-" + ">" + "-")
  }

  final def LT = rule {
    ("<" ~ !anyOf("=<") ~ Spacing).label("-" + "<" + "-")
  }

  final def SHIFTL = rule {
    ("<<" ~ !"=" ~ Spacing).label("-" + "<<" + "-")
  }

  final def SHIFTR = rule {
    (">>" ~ !anyOf("=>") ~ Spacing).label("-" + ">>" + "-")
  }

  final def LTE = rule {
    ("<=" ~ Spacing).label("-" + "<=" + "-")
  }

  final def GTE = rule {
    (">=" ~ Spacing).label("-" + ">=" + "-")
  }

  final def ANDAND = rule {
    ("&&" ~ Spacing).label("-" + "&&" + "-")
  }

  final def OROR = rule {
    ("||" ~ Spacing).label("-" + "||" + "-")
  }

  final def INC = rule {
    ("++" ~ Spacing).label("-" + "++" + "-")
  }

  final def DEC = rule {
    ("--" ~ Spacing).label("-" + "--" + "-")
  }

  final def QUERY = rule {
    ("?" ~ Spacing).label("-" + "?" + "-")
  }

  final def COLON = rule {
    (":" ~ Spacing).label("-" + ":" + "-")
  }

  final def PTRACCESS = rule {
    ("->" ~ Spacing).label("-" + "->" + "-")
  }

  // Predef
  final def HASH = rule {
    ("#" ~ Spacing).label("-" + "#" + "-")
  }

  // Punctuation

  final def COMMA = rule {
    ("," ~ Spacing).label("-" + "," + "-")
  }

  final def LPAR = rule {
    ("(" ~ Spacing).label("-" + "(" + "-")
  }

  final def RPAR = rule {
    (")" ~ Spacing).label("-" + ")" + "-")
  }

  final def LCURVEY = rule {
    ("{" ~ Spacing).label("-" + "{" + "-")
  }

  final def RCURVEY = rule {
    ("}" ~ Spacing).label("-" + "}" + "-")
  }

  final def LSQR = rule {
    ("[" ~ Spacing).label("-" + "[" + "-")
  }

  final def RSQR = rule {
    ("]" ~ Spacing).label("-" + "]" + "-")
  }

  final def SEMICOLON = rule {
    (";" ~ Spacing).label("-" + ";" + "-")
  }

  final def DOT = rule {
    ("." ~ Spacing).label("-" + "." + "-")
  }

  final def ELLIPSIS = rule {
    ("..." ~ Spacing).label("-" + "..." + "-")
  }


  ////////////////


  //final def BSR = Terminal(">>>", "=")
  //final def BSREQU = Terminal(">>>=")
  //final def LPOINT = Terminal("<")

  //final def RPOINT = Terminal(">")


  //final def AT = Terminal("@")



  //-------------------------------------------------------------------------
  //  Keywords
  //-------------------------------------------------------------------------

  // Simple lists are not here, e.g.
  // StorageClassSpecifier
  //     "typedef"
  //       | "extern"
  //       | "static"
  //       | "auto"
  //       | "register"
  //       | "inline"
  // TypeQualifier
  //       "const"
  //         | "volatile"
  // KeywordBasicTypeSpecifier
  //    "void"
  //    | "char"
  //    | "short"
  //    | "int"
  //    ...
  // StructOrUnion
  // ...
  //

  def Keyword = rule {
    (
      "goto" | "break" | "continue" | "return"
        | "switch" | "case" | "default"
        | "do" | "for" | "while"
        | "if" | "else"
        | "enum"
    ) ~ !LetterOrDigit
  }

  // N.B. These are not working with the Keyword declaration
  // from Java, but not in Scala
  final def GOTO = ("goto" ~ !LetterOrDigit ~ Spacing).label("-" + "goto" + "-")
  final def SIZEOF = ("sizeof" ~ !LetterOrDigit ~ Spacing).label("-" + "sizeof" + "-")

  final def BREAK = ("break" ~ !LetterOrDigit ~ Spacing).label("-" + "break" + "-")
  final def CONTINUE = ("continue" ~ !LetterOrDigit ~ Spacing).label("-" + "continue" + "-")
  final def RETURN = ("return" ~ !LetterOrDigit ~ Spacing).label("-" + "return" + "-")

  final def SWITCH = ("switch" ~ !LetterOrDigit ~ Spacing).label("-" + "switch" + "-")
  final def CASE = ("case" ~ !LetterOrDigit ~ Spacing).label("-" + "case" + "-")
  final def DEFAULT = ("default" ~ !LetterOrDigit ~ Spacing).label("-" + "default" + "-")

  final def DO = ("do" ~ !LetterOrDigit ~ Spacing).label("-" + "do" + "-")
  final def FOR = ("for" ~ !LetterOrDigit ~ Spacing).label("-" + "for" + "-")
  final def WHILE = ("while" ~ !LetterOrDigit ~ Spacing).label("-" + "while" + "-")

  final def IF = ("if" ~ !LetterOrDigit ~ Spacing).label("-" + "if" + "-")
  final def ELSE = ("else" ~ !LetterOrDigit ~ Spacing).label("-" + "else" + "-")

  final def ENUM = ("enum" ~ !LetterOrDigit ~ Spacing).label("-" + "enum" + "-")

  final def STATIC = ("static" ~ !LetterOrDigit ~ Spacing).label("-" + "static" + "-")

  //-------------------------------------------------------------------------
  // A.1.4 Universal character names
  //-------------------------------------------------------------------------


  def UniversalCharacterName  = rule {
    "\\u" ~ nTimes(4, HexadecimalDigit, Spacing)
    "\\U" ~ nTimes(8, HexadecimalDigit, Spacing)
  }

  //-------------------------------------------------------------------------
  // A.1.5 (Constants) Numeric Literals
  //-------------------------------------------------------------------------

  // From "A.1.5 Constants" from the draft.
  // See

  /** A literal number.
    *
    * Draft C Spec naming: `Constant`
    */
  def NumericLiteral = rule {
    (
      IntegerConstant
        | FloatingConstant
        //| EnumerationConstant
        | CharacterConstant
    ) ~ Spacing
  }

  // In my draft spec, the original is bad ordering. Check for Hex first, as it can positivly check the first chars as Digit ~ Letter. Then there is little difference between Octal and Decimal (the initial char being 0 or not) so try decimal, then octal.
  def IntegerConstant = rule {
    (
      HexadecimalConstant
        | DecimalConstant
        | OctalConstant
    ) ~ optional(IntegerSuffix)
  }

  /** A decimal literal.
    */
  def DecimalConstant = rule {
    ("1" - "9") ~ zeroOrMore("0" - "9")
  }

  /** An octal literal
    *
    * Octal starts with a "0"
    */
  def OctalConstant = rule {
    "0" ~ zeroOrMore(OctalDigit)
  }

  /** A hexadecimal literal.
    *
    * Draft C Spec: compounds `HexadeciamlPrefix` and `HexadecimalDigit`
    */
  def HexadecimalConstant = rule {
    HexadecimalPrefix ~ HexadecimalDigitSequence
  }

  def HexadecimalPrefix = rule {
    "0" ~ anyOf("xX")
  }

  def IntegerSuffix = rule {
    anyOf("uU") ~ optional(anyOf("lL") | ("ll" | "LL"))
    anyOf("lL") ~ optional(anyOf("uU"))
    ("ll" | "LL") ~ optional(anyOf("uU"))
  }

  def FloatingConstant = rule {(
    DecimalFloatingConstant
      | HexadecimalFloatingConstant
  )}

  def DecimalFloatingConstant = rule {
    (
      FractionalConstant ~ optional(ExponentPart)
        | DigitSequence ~ ExponentPart
    )  ~ optional(FloatingSuffix)
  }

  def HexadecimalFloatingConstant = rule {
    HexadecimalPrefix ~ (HexadecimalFractionalConstant | HexadecimalDigitSequence) ~ BinaryExponentPart ~ optional(FloatingSuffix)
  }

  def FractionalConstant = rule {(
    optional(DigitSequence) ~ "." ~ DigitSequence
      | DigitSequence ~ "."
  )}

  def ExponentPart = rule {
    anyOf("Ee") ~ optional(Sign) ~ DigitSequence
  }

  def HexadecimalFractionalConstant = rule {(
    optional(HexadecimalDigitSequence) ~ "." ~ HexadecimalDigitSequence
      | HexadecimalDigitSequence ~ optional(".")
  )}

  //
  // Character Numeric Literals
  //

  def CharacterConstant = rule {(
    optional(Letter) ~ "'" ~ oneOrMore(CChar) ~ "'"
  )}

  def CChar = rule {(
// Draft spec statement below, but I thought c has a strict character set?
// Ah well...
    // "any member of the source character set except
    // the single-quote ', backslash \, or new-line character"
    (!anyOf("'\\\n") ~ ANY)
      | EscapeSequence
  )}

  def EscapeSequence = rule {(
    SimpleEscapeSequence
      | OctalEscapeSequence
      | HexadecimalEscapeSequence
      | UniversalCharacterName
  )}

  def SimpleEscapeSequence = rule {
    "\\" ~ anyOf("\"'\\?abfnrtv")
  }

  def OctalEscapeSequence = rule {
    "\\" ~ (
      OctalDigit ~ OctalDigit ~ OctalDigit
        | OctalDigit ~ OctalDigit
        | OctalDigit
    )
  }

  def HexadecimalEscapeSequence = rule {
    "\\x" ~ oneOrMore(HexadecimalDigit)
  }




  //-------------------------------------------------------------------------
  // A.1.6 String literals
  //-------------------------------------------------------------------------

  // NB String/char literal escaping can be simpler than Java?
  // Parboiled Java blocks line ends. BISON C has an escape matching
  // '\\.', but I don't know if dot includes line ends, or what spec
  // is.  Currently, denying any true (non-inserted) newline control
  // chars

  /** A string literal
    *
    * This is a generic string literal, used in preprocess parsing.
    *
    * Draft C Spec: part of `HeaderNames`
    */
  def GenericStringLiteral = rule { "\"" ~ zeroOrMore( Escape | (!anyOf("\r\n\"\\") ~ ANY)) ~ "\"" }

  /** A string literal
    *
    * This is the usual string literal, allowing a preceeding `Letter` as modifier.
    */
  def StringLiteral = rule(SuppressSubnodes) { optional(Letter) ~ GenericStringLiteral ~ Spacing}

  /** A string delimited with angle brackets
    *
    * Used in preprocess parsing.
    *
    * Draft C Spec: part of `HeaderNames`
    */
  def RefLiteral = rule(SuppressSubnodes) { "<" ~ zeroOrMore( !(">") ~ ANY ) ~ ">" }


  //-------------------------------------------------------------------------
  // A.1.7 Punctuators
  //-------------------------------------------------------------------------

  def Punctuator  = rule {(
    "=" ~ optional("=")
      | "." ~ optional("..")
      | "[" | "]" | "(" | ")" | "{" | "}"
      | "-" ~ optional(anyOf("=->"))
      | "|" ~ optional(anyOf("|="))
      | "&" ~ optional(anyOf("&="))
      | "^" ~ optional("=")
      | "/" ~ optional("=")
      | "+" ~ optional(anyOf("=+"))
      | "*" ~ optional("=")
      | "%" ~ optional("=:>")
      | "!" ~ optional("=")
      | "<" ~ optional(anyOf("=<%:"))
      | ">" ~ optional(anyOf("=>"))
      | ":" ~ optional(anyOf(">"))
      | "#" ~ optional("#")

    | "~" | "?" |  ";"
      | "<<="
      | ","
      | "%:%:"
      | ">>="
  )}

  //-------------------------------------------------------------------------
  // A.1.9 Preprocessing numbers
  //-------------------------------------------------------------------------

  def PPNumber = rule {
    oneOrMore(
      "."
        | Digit
        //| IdentifierNondigit ?
        | (Exponent | BinaryExponent) ~ Sign
    ) ~ Spacing
  }




  //-------------------------------------------------------------------------
  // A.2 Phrase structure grammar
  //-------------------------------------------------------------------------

  /////////////////
  // Entry point //
  /////////////////

  /** The entry point for a full-rule parse match.
    *
    * Parboiled idomatic alias for TranslationUnit
    * @see [[TranslationUnit]]
    */
  def Root = rule {
    TranslationUnit
  }


  //-------------------------------------------------------------------------
  // A.2.1 Expressions
  //-------------------------------------------------------------------------

  /** Matches any expression which can fully evaluate within itself
    *
    * A different name might be a `primitive' expression. The rule
    * also matches complex resolving expressions in brackets.
    * 
    * @see [[Identifier]], [[NumericLiteral]], [[StringLiteral]], and [[CharLiteral]], all of which match. 
    */
  def PrimaryExpression = rule {(
    Identifier
      | NumericLiteral
      | StringLiteral
      //| CharLiteral
      | LPAR ~ Expression ~ RPAR
  )}

  
  /** Matches the set of allowable operators after an expression
    *
    * This includes such ideas as dot and pointer access to
    * structs/unions, post-decrements, array referencing, etc.
    *
    * e.g. {{{ [x-1] }}}, {{{ context->color }}}
    *
    * Draft C Spec: part of `PostfixExpression`
    * @see [[PostfixExpression]]
    */
  def PostfixExpressionOperator = rule {(
    LSQR ~ Expression ~ RSQR
      | LPAR ~ optional(ArgumentExpressionList) ~ RPAR
      | DOT ~ Identifier
      | PTRACCESS ~ Identifier
      | INC
      | DEC
  )}
  
  /** Matches a primary expression, allowing several postfixed operations.
    *
    * The postfixed operations include dot and pointer access to
    * structs/unions, inc and dec operators, and parameter calls.
    * 
    * @see [[PostfixExpressionOperator]]
    */
  def PostfixExpression : Rule0 = rule {
    (
      PrimaryExpression
        | LPAR ~ TypeName ~ RPAR ~ LSQR ~ InitializerList ~ optional(COMMA) ~ RSQR
    ) ~ zeroOrMore(PostfixExpressionOperator)
  }

  def ArgumentExpressionList = rule {
    AssignmentExpression ~ zeroOrMore( COMMA ~ AssignmentExpression )
  }


  /** Matches expressions with optional prefix operators
    *
    * If the expression is incomplete (only sizeof(...) is not), this
    * rule calls follows with `PostfixExpression'.
    *
    * e.g. {{{ &... }}}, {{{ ++... }}}, {{{ !... }}}, {{{ sizeof(int) }}}
    */
  def UnaryExpression : Rule0 = rule {
    zeroOrMore( INC | DEC | SIZEOF ) ~ (
      PostfixExpression
        | UnaryOperator ~ CastExpression
        | SIZEOF ~ LPAR ~ TypeName ~ RPAR
    )
  }

  /** Matches single char prefix operators
    *
    * e.g. {{{!..., &...}}}
    */
  def UnaryOperator = rule {
    (
      AND
        | STAR
        | PLUS
        | MINUS
        | TITHE
        | NOT
    )
  }

  /** Matches casts, then a following expression
    * e.g. {{{ (int *) x++ }}} 
    */
  def CastExpression = rule {(
    zeroOrMore(LPAR ~ TypeName ~ RPAR) ~ UnaryExpression
  )}

  def MultiplicativeExpression : Rule0 = rule {
    CastExpression ~ zeroOrMore( (STAR | DIV | MOD) ~ CastExpression)
  }

  def AdditiveExpression : Rule0 = rule {
    MultiplicativeExpression ~ zeroOrMore( (PLUS | MINUS) ~ MultiplicativeExpression )
  }

  def ShiftExpression : Rule0 = rule {
    AdditiveExpression ~ zeroOrMore( (SHIFTL | SHIFTR) ~ AdditiveExpression )
  }

  def RelationalExpression : Rule0 = rule {
    ShiftExpression ~ zeroOrMore( (LT | GT | LTE | GTE) ~ ShiftExpression )
  }

  def EqualityExpression : Rule0 = rule {
    RelationalExpression ~ zeroOrMore( ("==" | "!=")  ~ Spacing ~ RelationalExpression )
  }

  def AndExpression = rule {
    EqualityExpression ~ zeroOrMore(AND ~ EqualityExpression)
  }

  def ExclusiveOrExpression = rule {
    AndExpression ~ zeroOrMore(XOR ~ AndExpression)
  }

  def InclusiveOrExpression = rule {
    ExclusiveOrExpression ~ zeroOrMore(OR ~ ExclusiveOrExpression)
  }


  def LogicalAndExpression = rule {
    InclusiveOrExpression ~ zeroOrMore(ANDAND ~ InclusiveOrExpression)
  }


  /** Matches a '||' binary operation.
    */
  def LogicalOrExpression = rule {
    LogicalAndExpression ~ zeroOrMore(OROR ~ LogicalAndExpression)
  }


  /** Matches optional trailing conditional expressions
    *
    * The three way 'c' operator
    * e.g. {{{a ? b : c}}}
    */
  def ConditionalExpression : Rule0 = rule {(
    LogicalOrExpression ~ optional(QUERY ~ Expression ~ COLON ~ ConditionalExpression)
  )}


  /** Matches an assigning expression
    *
    * With adaption, this rule is used after the assign symbol (see
    * `Initializer'). The rule is also used directly in `Expression`.
    *
    * Can be as simple as a numeric constant, or a complex resolving
    * expression.
    *
    * e.g. {{{ "1234, lets go" }}}, {{{ --a != b + c }}}
    * @see [[Initializer]]
    */
  def AssignmentExpression = rule {
    zeroOrMore(UnaryExpression ~ AssignmentOperator) ~ ConditionalExpression
  }

  // Aside from "=", these are the longest possible symbolic operators of their kind,
  // so need no post-match tests.
  def AssignmentOperator = rule {
    (
      ("=" ~ !"=" )
        | "*="
        | "/="
        | "%="
        | "+="
        | "-="
        | "<<="
        | ">>="
        | "&="
        | "^="
        | "|="
    ) ~ Spacing
  }

  /** Matches a list of assignment expressions
    * 
    * e.g. {{{ a=4, b=6, c=8}}} 
    */
  def Expression = rule {
    AssignmentExpression ~ zeroOrMore( COMMA ~ AssignmentExpression )
  }

  /** Any conditional expression
    * Conditional expressions cascade into constants.
    *
    * @see [[ConstantExpression]]
    */
  def ConstantExpression = rule {
    ConditionalExpression
  }



  //-------------------------------------------------------------------------
  // A.2.2 Declarations
  //-------------------------------------------------------------------------

  /** Declare a value
    *
    * e.g. {{{ int a = "bonsai"; }}} {{{ int a; }}}
    */
  def Declaration = rule(SuppressSubnodes) {
    DeclarationSpecifiers ~ optional(InitDeclaratorList) ~ SEMICOLON
  }


  /** All pre-specifiers to a declaration
    *
    * e.g. {{{ static const int }}}
    *
    * Must end in a type, e.g. {{{ int }}}
    *
    * Draft C Spec note: The spec allows these elements to be
    * unordered. To avoid ambiguity between types and type names (this
    * is a PEG parser, lexer feedback and symbol tables are not
    * usually present), this rule demands specifiers follow
    * qualifiers. Thus the rule will fail on some written-to-spec 'c'
    * code. However, most 'c' code will parse (it is eccentric to
    * write "Context const...").
    * 
    * @see [[NonStorageTypeSpecifier]]
    */
  def DeclarationSpecifiers = rule(SuppressSubnodes) {
    zeroOrMore(TypeQualifier | StorageClassSpecifier) ~ TypeSpecifier
  }



  def InitDeclaratorList = rule {
    InitDeclarator ~ zeroOrMore( COMMA ~ InitDeclarator )
  }

  /** Matches declared item assignment.
    *
    * This Rule gathers the `Declarator', the assignment symbol `=',
    * and the `Initializer' to the right.
    *
    * e.g. {{{rack = 19}}}
    */
  def InitDeclarator = rule {
    Declarator ~ optional( ASSIGN ~ Initializer )
  }
  
  /** Matches keywords for storage directions
    *
    * e.g. {{{ static }}}, {{{ extern }}}
    *
    * Draft C Spec: The function-specifier "inline" is included here
    */
  def StorageClassSpecifier = rule {
    (
      "typedef"
        | "extern"
        | "static"
        | "auto"
        | "register"
        | "inline"
    ) ~ !LetterOrDigit ~ Spacing
  }

  /** Matches type keywords.
    *
    * e.g. {{{ void }}}, {{{ char}}}
    *
    * Draft C Spec: part of `TypeSpecifier`
    */
  def KeywordBasicTypeSpecifier  = rule {
    (
      "void"
        | "char"
        | "short"
        | "int"
        | "long"
        | "float"
        | "double"
        | "signed"
        | "unsigned"
    ) ~ !LetterOrDigit ~ Spacing
  }

  /** Matches keyworded possibilities for a type name
    *
    * This could be some keyword type primitives, a struct, or enum
    *
    * Draft C Spec: part of TypeSpecifier
    */
  def KeywordTypeSpecifier = rule {
    (
      oneOrMore(KeywordBasicTypeSpecifier)
        | StructOrUnionSpecifier
        | EnumSpecifier
    )
  }

  /** Matches possibilities for a type name
    *
    * This could be a keyword primitive, a struct, enum, or, as a last
    * guess, any identity/symbol token.
    */
  def TypeSpecifier = rule {
    (
      KeywordTypeSpecifier
        | Identifier
    )
  }

  def StructOrUnionSpecifier = rule {
    StructOrUnion ~ optional(Identifier) ~ optional(LCURVEY ~ StructDeclarationList ~ RCURVEY)
  }

  def StructOrUnion = rule {
    (
      "struct"
        | "union"
    ) ~ !LetterOrDigit ~ Spacing
  }
  
  
  def StructDeclarationList = rule {
    oneOrMore(StructDeclaration)
  }
  
  def StructDeclaration = rule {
    NonStorageTypeSpecifier ~ StructDeclaratorList ~ SEMICOLON
  }

  /** Matches a specification for a type.
    *
    * This rule has no matching for [[StorageClassSpecifier]]. Used
    * for type specification and struct declaration. Only used in
    * casts, `sizeof` and `struct`s.
    *
    * e.g. {{{ volatile long long }}}, {{{ const FMagic }}}
    * 
    * Draft C Spec naming: `SpecifierQualifierList`
    * 
    * Draft C Spec note: The spec is loose at this point, presumably
    * to allow lexer feedback and similar parsing strategies. To avoid
    * ambiguity between types and type names (this is a PEG parser,
    * lexer feedback and symbol tables are not present), this rule
    * demands specifiers follow qualifiers. Thus the rule will fail on
    * some written-to-spec 'c' code. However, most 'c' code will parse
    * (it is eccentric to write "Context const...").
    * 
    * @see [[DeclarationSpecifiers]] for matching with storage.
    */
  def NonStorageTypeSpecifier : Rule0 = rule {
    zeroOrMore(TypeQualifier) ~ TypeSpecifier
  }

  def StructDeclaratorList = rule {
    StructDeclarator ~ zeroOrMore( COMMA ~ StructDeclarator )
  }


  def StructDeclarator = rule {(
    Declarator ~ optional(COLON ~ ConstantExpression)
      | COLON ~ ConstantExpression
  )}

  def EnumSpecifier = rule {
    ENUM ~ (
      Identifier ~ optional(LCURVEY ~ EnumeratorList ~ optional(COMMA) ~ RCURVEY)
        | LCURVEY ~ EnumeratorList ~ optional(COMMA) ~ RCURVEY
    )
  }

  def EnumeratorList = rule {
    Enumerator ~ zeroOrMore( COMMA ~ Enumerator )
  }

  /** Matches an enumerator
    *
    * Draft C Spec: compunds `EnumerationConstant` as an `Identifier`
    */
  def Enumerator = rule {
    Identifier ~ zeroOrMore( ASSIGN ~ ConstantExpression)
  }

  def TypeQualifier = rule {
    (
      "const"
        | "volatile"
        | "restrict"
    ) ~ !LetterOrDigit ~ Spacing
  }

  /** Opens declarative brackets for arrays and methods
    *
    * Opens method parameter brackets, amongst others,
    *
    * e.g. {{{ [4]; }}}, {{{ (); }}}
    */
  def DirectDeclaratorParameters = rule {(
    (LSQR ~ (
      zeroOrMore(TypeQualifier) ~ optional(STAR | AssignmentExpression)
        |  STATIC ~ zeroOrMore(TypeQualifier) ~ AssignmentExpression
        | oneOrMore(TypeQualifier) ~ STATIC ~ AssignmentExpression
    ) ~ RSQR)
      | (LPAR ~ optional(ParameterTypeList | IdentifierList) ~ RPAR)
  )}

  /** Matches a declaration of value (including functions) of some kind.
    *
    * e.g {{{ myArray[4] }}}, {{{ *func() }}}
    */
  def Declarator : Rule0 = rule { optional(PointerList) ~ DirectDeclarator }
  

  /** The main identifier and parameters of a declaration.
    *
    * e.g. {{{ hill[5] }}}, {{{ (...) }}}
    *
    * @see [[Declarator]] for handling this with a pointer
    */
  def DirectDeclarator = rule {
    (Identifier | (LPAR ~ Declarator ~ RPAR)) ~ zeroOrMore(DirectDeclaratorParameters)
  }

  /** Matches a star used as a pointer operator, with optional qualifiers
    *
    * e.g. {{{ * const }}}
    *
    * Draft C Spec: `Pointer` compounds `TypeQualifierList` from the spec.
    */
  // Is this right? No...
  def Pointer = rule {
    oneOrMore( STAR ~ zeroOrMore(TypeQualifier) )
  }

  /** Matches a list of stars used as a pointer operator, with optional qualifiers
    *
    * e.g. {{{ ** const }}}
    *
    * Draft C Spec: `PointerList` compounds `Pointer` and `PointerList` from the spec.
    */
  def PointerList = rule {
    oneOrMore(Pointer)
  }

  /** Matches parameter declarations followed by an ellipse.
    *
    * e.g. {{{ char grits, ... }}}
    *
    * @see [[ParameterList]] for the contents of the list.
    */
  def ParameterTypeList = rule {
    ParameterList ~ optional(COMMA ~ ELLIPSIS)
  }

  /** Matches a list of parameter declarations.
    *
    * As used in the declaration of parameters,
    *
    * e.g. {{{int in, int out}}}
    */
  def ParameterList = rule {
    ParameterDeclaration ~ zeroOrMore( COMMA ~ ParameterDeclaration )
  }

  /** Matches the declaration of a single parameter
    * 
    * e.g. {{{ const int orientation }}}
    */
  def ParameterDeclaration = rule {
    DeclarationSpecifiers ~ optional( Declarator | AbstractDeclarator)
  }
  


  /** Matches a list of identifiers
    *
    * Used in some declarations.
    */
  def IdentifierList = rule {
    Identifier ~ zeroOrMore( COMMA ~ Identifier )
  }

  /** Matches a typename, accounting for prefixed qualifiers
    *
    * Only used in casts and `sizeof`
    *
    * e.g. {{{ const * char }}}
    */
  // Is this right? Why have multiple qualifier lists?
  def TypeName = rule {
    NonStorageTypeSpecifier ~ optional(AbstractDeclarator)
  }
  


  // Abstract declarators //

  /** Matches pointers (optionally followed by bracketed declarators), or bracketed declarators.
    *
    * e.g. {{{ * }}} {{{ *(int) }}} {{{ (char* needle, Stack haystack) }}}
    */
  def AbstractDeclarator: Rule0 = rule {(
    PointerList ~ optional(DirectAbstractDeclaratorList)
      | DirectAbstractDeclaratorList
  )}


  /** Matches declarators in some form of bracketing
    *
    * e.g. {{{ () }}}, {{{ ("beak") }}}, {{{ (int, short) }}}, {{{ [a + 1] }}}
    *
    * Draft C Spec naming: this rule is NOT `DirectAbstractDeclarator` from
    * the spec. 
    * @see [[DirectAbstractDeclaratorList]]
    */
  def DirectAbstractDeclarator = rule {(
    LPAR ~ optional(AbstractDeclarator | ParameterTypeList) ~ RPAR
      |  LSQR ~ optional(STAR | AssignmentExpression) ~ RSQR
  )}

  /** Matches a list of bracketed declarators
    *
    * e.g. {{{ [a][b + 1] }}}
    *
    * Draft C Spec naming: `DirectAbstractDeclarator`
    */
  def DirectAbstractDeclaratorList = rule {
    oneOrMore(DirectAbstractDeclarator)
  }




  //-------------------------------------------------------------------------
  //  (6.7.8) Initializers
  //-------------------------------------------------------------------------

  /** Matches any valid statement to the right of an assignment operater.
    */
  // Don't know what the trailing comma is for, but it's spec :)
  def Initializer : Rule0 = rule {(
    AssignmentExpression
      | LCURVEY ~ InitializerList ~ optional(",") ~ RCURVEY
  )}

  /** Matches a list of assignments.
    */
  def InitializerList = rule {
    optional(Designation) ~ Initializer ~ oneOrMore(COMMA ~ optional(Designation) ~ Initializer)
  }



  def Designation = rule {
    oneOrMore(Designator) ~ ASSIGN
  }

  def Designator = rule {(
    LSQR ~ ConstantExpression ~ RSQR
      | DOT ~ Identifier
  )}



  //-------------------------------------------------------------------------
  // A.2.3 Statements
  //-------------------------------------------------------------------------

  def Statement : Rule0 = rule {(
    LabeledStatement
      | CompoundStatement
      | ExpressionStatement
      | SelectionStatement
      | IterationStatement
      | JumpStatement
  )}

  /** Matches elements in a switch statement
    *
    * e.g. {{{ case ...: ... default ... : ... }}}
    */
  def LabeledStatement = rule {(
    Identifier ~ COLON ~ Statement
      | CASE ~ ConstantExpression ~ COLON ~ Statement
      | DEFAULT ~ COLON ~ Statement
  )}


  /** Surrounds the statements inside a code block.
    *
    * This general rule is used for methods, and standalone as an item
    * in statements.
    *
    * Draft C Spec: compunds the `blockItem` and `blockItemList` rules
    */
  def CompoundStatement = rule {
    LCURVEY ~ zeroOrMore( Declaration | Statement) ~ RCURVEY
  }


  /** Matches a full statement
    *
    * An expression and terminating semi-colon
    * e.g. {{{ [1] = 4; }}}
    */
  def ExpressionStatement = rule {
    optional(Expression) ~ SEMICOLON
  }

  /** Matches branching statements
    *
    * An `if` or `switch` statement
    * e.g. {{{ if(x--){ i++; } }}}
    */
  def SelectionStatement = rule {(
    IF ~ LPAR ~ Expression ~ RPAR ~ Statement ~ optional( ELSE ~ Statement )
      | SWITCH ~ LPAR ~ Expression ~ RPAR ~ Statement
  )}

  /** Matches loop statements
    */
  def IterationStatement = rule {(
    WHILE ~ LPAR ~ Expression ~ RPAR ~ Statement
      | DO ~ Statement ~ WHILE ~ LPAR ~ Expression ~ RPAR ~ SEMICOLON
      | FOR ~ LPAR ~ ExpressionStatement ~ ExpressionStatement ~ optional( Expression ) ~ RPAR ~ Statement
  )}


  /** Matches jump statements
    *
    * e.g. {{{ break; }}}, {{{ return; }}}
    */
  def JumpStatement = rule {
    (
      (GOTO ~ Identifier)
        | CONTINUE
        | BREAK
        | RETURN ~ optional(Expression)
    ) ~ SEMICOLON
  }



  //-------------------------------------------------------------------------
  // A.2.4 External definitions
  //-------------------------------------------------------------------------


  /** Matches a declaration or a preprocessor declaration
    *
    * Root of the rule tree.
    * @see [[Root]]
    */
  def TranslationUnit = rule {
    zeroOrMore(Spacing ~ (ExternalDeclaration | PreprocessDeclaration))
  }

  /** Matches a function definition or a declaration
    */
  def ExternalDeclaration = rule(SuppressSubnodes) {
    FunctionDefinition | Declaration
  }

  /** Matches a function
    *
    * e.g. {{{ static void left(int *x, int *y) { x*--; } }}}
    */
  // example not working?
  def FunctionDefinition = rule(SuppressSubnodes)  {
    optional(DeclarationSpecifiers) ~ Declarator ~ optional(DeclarationList) ~ CompoundStatement
  }
  

  /** Matches a list of declarations
    *
    * e.g. "const int jimmy, static Greep cricket"  
    */
  def DeclarationList = rule { oneOrMore( Declaration ) }



  //-------------------------------------------------------------------------
  //  Preprocessor Keywords
  //-------------------------------------------------------------------------

  final def IFDEF = ("ifdef" ~ !LetterOrDigit ~ Spacing).label("-" + "ifdef" + "-")
  final def IFNDEF = ("ifndef" ~ !LetterOrDigit ~ Spacing).label("-" + "ifndef" + "-")
  final def ELIF = ("elif" ~ !LetterOrDigit ~ Spacing).label("-" + "elif" + "-")
  final def ENDIF = ("endif" ~ !LetterOrDigit ~ Spacing).label("-" + "endif" + "-")
  final def INCLUDE = ("include" ~ !LetterOrDigit ~ Spacing).label("-" + "include" + "-")
  final def DEFINE = ("define" ~ !LetterOrDigit ~ Spacing).label("-" + "define" + "-")
  final def UNDEF = ("undef" ~ !LetterOrDigit ~ Spacing).label("-" + "undef" + "-")
  final def LINE = ("line" ~ !LetterOrDigit ~ Spacing).label("-" + "line" + "-")
  final def PRAGMA = ("pragma" ~ !LetterOrDigit ~ Spacing).label("-" + "pragma" + "-")
  final def ERROR = ("error" ~ !LetterOrDigit ~ Spacing).label("-" + "error" + "-")



  //-------------------------------------------------------------------------
  //  A.3 Preprocessor
  //-------------------------------------------------------------------------

  def PreprocessDeclaration = rule(SuppressSubnodes){ "#" ~ (
    Include
      | Other
  ) ~ Spacing
  }

  def Include = rule { "include" ~ oneOrMore(!(anyOf("\"<")) ~ ANY) ~ (
    GenericStringLiteral
      | RefLiteral
  ) ~ Line
  }



  // Not good enough. Needs to find a line not ending in a continuation symbol...
  def Other = rule { Line }


  /** A token used in preprocessing statements.
    * 
    * In the Draft spec, tokens can be separated by
    * whitespace. Whitespace is not allowed anywhere else aside from
    * inside brackets, for header names and string literals.
    *
    * Draft C Spec: `GenericStringLiteral` and `RefLiteral` compund to `HeaderName`
    */
  def PreprocessingToken = rule {(
    GenericStringLiteral
      | RefLiteral
      | Identifier
      | PPNumber
      | CharLiteral
      | StringLiteral
      | Punctuator
      //each non-white-space character that cannot be one of the above
      | (!(WhitespaceChar) ~ ANY)
  )}




  //preprocessing-file:

  def Group : Rule0 = rule { oneOrMore(GroupPart) }

  def GroupPart = rule {(
    IfSection
      | ControlLine
      | TextLine
      | (HASH ~ NonDirective)
  )}

  def IfSection = rule {
    IfGroup ~ optional(ElifGroups) ~ optional(ElseGroup) ~ EndifLine
  }

  def IfGroup : Rule0 = rule {
    HASH ~ (
      IF ~ ConstantExpression
        | (IFDEF | IFNDEF) ~ Identifier ~ LineEnd
    ) ~ LineEnd ~ optional(Group)
  }

  def ElifGroups = rule {
    ElifGroup ~ zeroOrMore(ElifGroup)
  }


  def ElifGroup = rule {
    HASH ~ ELIF ~ ConstantExpression ~ LineEnd ~ optional(Group)
  }


  def ElseGroup = rule {
    HASH ~ ELSE ~ LineEnd ~ optional(Group)
  }


  def EndifLine = rule { HASH ~ ENDIF ~ LineEnd }


  def ControlLine = rule {
    HASH ~ optional(
      INCLUDE ~ PPTokens
        | DEFINE ~ Identifier ~ (
          ReplacementList
            | LPAR ~ (
              ELLIPSIS ~ RPAR
                | optional(IdentifierList ~ optional(COMMA ~ ELLIPSIS)) ~ RPAR ~ ReplacementList
            )
        )
        | UNDEF ~ Identifier
        | LINE ~ PPTokens
        | ERROR ~ optional(PPTokens)
        | PRAGMA ~ optional(PPTokens)
    ) ~ LineEnd
  }


  def TextLine = rule {
    optional(PPTokens) ~ LineEnd
  }


  def NonDirective = rule {
    PPTokens ~ LineEnd
  }


  // Humm?
  /*
   def MacroLParen = rule {
   //a ( character not immediately preceded by white-space
   "("
   }
   */

  def ReplacementList = rule {
    optional(PPTokens)
  }

  def PPTokens = rule {
    PreprocessingToken ~ zeroOrMore(PreprocessingToken)
  }


}//CParser
