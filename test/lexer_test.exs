defmodule LexerTest do
  use ExUnit.Case
  alias MonkeyInterpreter.{Lexer, Token}
  doctest Lexer

  test "lexer can lex a basic program and check every token using all_tokens" do
    input = ~s'let five = 5;
let ten = 10;

let add = fn(x, y) {
  x + y;
};

let result = add(five, ten);
!-/*5;
5 < 10 > 5;

if (5 < 10) {
	return true;
} else {
	return false;
}

10 == 10;
10 != 9;
"foobar";
"foo bar";
[1, 2];
'

    expected_tokens = [
      Token.init(:let, "let"),
      Token.init(:ident, "five"),
      Token.init(:assign, "="),
      Token.init(:int, "5"),
      Token.init(:semicolon, ";"),
      Token.init(:let, "let"),
      Token.init(:ident, "ten"),
      Token.init(:assign, "="),
      Token.init(:int, "10"),
      Token.init(:semicolon, ";"),
      Token.init(:let, "let"),
      Token.init(:ident, "add"),
      Token.init(:assign, "="),
      Token.init(:function, "fn"),
      Token.init(:lparen, "("),
      Token.init(:ident, "x"),
      Token.init(:comma, ","),
      Token.init(:ident, "y"),
      Token.init(:rparen, ")"),
      Token.init(:lbrace, "{"),
      Token.init(:ident, "x"),
      Token.init(:plus, "+"),
      Token.init(:ident, "y"),
      Token.init(:semicolon, ";"),
      Token.init(:rbrace, "}"),
      Token.init(:semicolon, ";"),
      Token.init(:let, "let"),
      Token.init(:ident, "result"),
      Token.init(:assign, "="),
      Token.init(:ident, "add"),
      Token.init(:lparen, "("),
      Token.init(:ident, "five"),
      Token.init(:comma, ","),
      Token.init(:ident, "ten"),
      Token.init(:rparen, ")"),
      Token.init(:semicolon, ";"),
      Token.init(:bang, "!"),
      Token.init(:minus, "-"),
      Token.init(:slash, "/"),
      Token.init(:asterisk, "*"),
      Token.init(:int, "5"),
      Token.init(:semicolon, ";"),
      Token.init(:int, "5"),
      Token.init(:lt, "<"),
      Token.init(:int, "10"),
      Token.init(:gt, ">"),
      Token.init(:int, "5"),
      Token.init(:semicolon, ";"),
      Token.init(:if, "if"),
      Token.init(:lparen, "("),
      Token.init(:int, "5"),
      Token.init(:lt, "<"),
      Token.init(:int, "10"),
      Token.init(:rparen, ")"),
      Token.init(:lbrace, "{"),
      Token.init(:return, "return"),
      Token.init(true, "true"),
      Token.init(:semicolon, ";"),
      Token.init(:rbrace, "}"),
      Token.init(:else, "else"),
      Token.init(:lbrace, "{"),
      Token.init(:return, "return"),
      Token.init(false, "false"),
      Token.init(:semicolon, ";"),
      Token.init(:rbrace, "}"),
      Token.init(:int, "10"),
      Token.init(:eq, "=="),
      Token.init(:int, "10"),
      Token.init(:semicolon, ";"),
      Token.init(:int, "10"),
      Token.init(:not_eq, "!="),
      Token.init(:int, "9"),
      Token.init(:semicolon, ";"),
      Token.init(:string, "foobar"),
      Token.init(:semicolon, ";"),
      Token.init(:string, "foo bar"),
      Token.init(:semicolon, ";"),
      Token.init(:lbracket, "["),
      Token.init(:int, "1"),
      Token.init(:comma, ","),
      Token.init(:int, "2"),
      Token.init(:rbracket, "]"),
      Token.init(:semicolon, ";"),
      Token.init(:eof, "")
    ]

    lexer = Lexer.init(input)

    # Go over every expected token, smuggling along the updated lexer as the "acc" in a reduce/fold.
    # Check that the returned token matches each expected token.
    # expected_tokens
    # |> Enum.reduce(lexer, fn expected_token, l ->
    #   assert {new_lexer, ^expected_token} = Lexer.next_token(l)
    #   new_lexer
    # end)

    # This is a nicer way to do it, just grab all the tokens and compare them.
    assert expected_tokens == Lexer.all_tokens(lexer)
  end
end
