/*
NetLisp is the reference implementation for a language similar to
Scheme, also called NetLisp. This implementation is both interpreted
and compiled, targetting the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005 Adam Milazzo

This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License
as published by the Free Software Foundation; either version 2
of the License, or (at your option) any later version.
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.
You should have received a copy of the GNU General Public License
along with this program; if not, write to the Free Software
Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
*/

using System;
using System.Collections;
using System.IO;
using System.Text;
using System.Text.RegularExpressions;
using Scripting;

namespace NetLisp.Backend
{

public sealed class SyntaxObject
{ public SyntaxObject(string file, string code, object sexp, Position start, Position end)
  { File=file; Code=code; Sexp=sexp; Start=start; End=end;
  }
  public object Sexp;
  public string File, Code;
  public Position Start, End;
}

public enum Token
{ None, EOF, Literal, Symbol, Vector, Splice,
  LParen, RParen, LBracket, RBracket, LCurly, RCurly, Quote, BackQuote, Comma, Period
}

public sealed class Parser
{ public Parser(Stream data) : this("<unknown>", data) { }
  public Parser(string source, Stream data) : this(source, new StreamReader(data), false) { }
  public Parser(string source, TextReader data) : this(source, data, false) { }
  public Parser(string source, TextReader data, bool autoclose)
  { sourceFile = source; this.data = data.ReadToEnd();
    if(autoclose) data.Close();
    NextToken();
  }
  public Parser(string source, string data)
  { sourceFile=source; this.data=data;
    NextToken();
  }

  public string SourceCode { get { return data; } }
  public string SourceFile { get { return sourceFile; } }

  public object Parse() { return Parse(true); }
  public object Parse(bool returnSyntax)
  { Pair list=null, tail=null;
returnSyntax = false; // TODO: remove this

    Position start = currentPos;
    object obj;
    while(token!=Token.EOF)
    { obj = ParseOne(returnSyntax);
      Pair next = new Pair(obj, null);
      if(list==null) list=tail=next;
      else { tail.Cdr=next; tail=next; }
    }

    if(list!=null && list.Cdr==null) return list.Car;

    obj = list==null ? null : (object)new Pair(Symbol.Get("begin"), list);
    return returnSyntax ? new SyntaxObject(sourceFile, data, obj, start, currentPos) : obj;
  }

  public object ParseOne() { return ParseOne(true); }
  public object ParseOne(bool returnSyntax)
  { object ret;
returnSyntax = false; // TODO: remove this
    Position start = currentPos;
    switch(token)
    { case Token.LParen: case Token.LBracket:
      { Token end = token==Token.LParen ? Token.RParen : Token.RBracket;
        if(NextToken()==end) { NextToken(); ret=null; }
        else
        { ArrayList items = new ArrayList();
          object dot = null;
          do
          { items.Add(ParseOne(returnSyntax));
            if(TryEat(Token.Period)) { dot=ParseOne(returnSyntax); break; }
          }
          while(token!=end && token!=Token.EOF);
          if(items.Count==0 && dot!=null) throw SyntaxError("malformed dotted list");
          Eat(end);
          ret = LispOps.DottedList(dot, (object[])items.ToArray(typeof(object)));
        }
        break;
      }
      case Token.Symbol:
      { object val=value;
        NextToken();
        ret = Symbol.Get((string)val);
        break;
      }
      case Token.Literal:
      { ret = value;
        NextToken();
        break;
      }
      case Token.BackQuote: NextToken(); ret = LispOps.List(quasiSym, ParseOne(returnSyntax)); break;
      case Token.Comma: NextToken(); ret = LispOps.List(unquoteSym, ParseOne(returnSyntax)); break;
      case Token.Quote: NextToken(); ret = LispOps.List(quoteSym, ParseOne(returnSyntax)); break;
      case Token.Splice: NextToken(); ret = LispOps.List(spliceSym, ParseOne(returnSyntax)); break;
      case Token.Vector:
      { ArrayList items = new ArrayList();
        NextToken();
        while(!TryEat(Token.RParen)) items.Add(ParseOne(returnSyntax));
        ret = LispOps.List2(vectorSym, (object[])items.ToArray(typeof(object)));
        break;
      }
      case Token.EOF: return EOF;
      default: throw SyntaxError("unexpected token: "+token);
    }

    while(token==Token.LCurly) // obj{Prop a0 a1 ...} -> ((.member obj "Prop") .last a0 a1 ...)
    { NextToken();
      Expect(Token.Symbol);

      Pair tail = new Pair(dotLastSym, null);
      ret = new Pair(new Pair(memberSym, new Pair(ret, new Pair((string)value, null))), tail);
      NextToken();
      while(token!=Token.RCurly)
      { Pair next = new Pair(ParseOne(returnSyntax), null);
        tail.Cdr=next; tail=next;
      }
      NextToken();
    }

    return returnSyntax ? new SyntaxObject(sourceFile, data, ret, start, endPos) : ret;
  }

  public static Parser FromFile(string filename) { return new Parser(filename, new StreamReader(filename), true); }
  public static Parser FromStream(Stream stream) { return new Parser("<stream>", new StreamReader(stream)); }
  public static Parser FromString(string text) { return new Parser("<string>", text); }
  public static Parser FromString(string source, string text) { return new Parser(source, text); }

  public static bool IsDelimiter(char c)
  { if(char.IsWhiteSpace(c)) return true;
    switch(c)
    { case '(': case ')': case '[': case ']': case '{': case '}':
      case '#': case '`': case ',': case '\'': case'\0':
        return true;
      default: return false;
    }
  }

  public static object ParseNumber(string str, int radix) { return ParseNum(str, "", radix, '\0'); }

  public static readonly object EOF = new Singleton("<EOF>");

  void Eat(Token type) { if(token!=type) Unexpected(token, type); NextToken(); }
  void Expect(Token type) { if(token!=type) Unexpected(token); }

  /*
  \newline  Ignored
  \\        Backslash
  \"        Double quotation mark
  \'        Single quotation mark
  \n        Newline
  \t        Tab
  \r        Carriage return
  \b        Backspace
  \e        Escape
  \a        Bell
  \f        Form feed
  \v        Vertical tab
  \xHH      Up to 2 hex digits -> byte value
  \uHHHH    Up to 4 hex digits -> 16-bit unicode value
  \cC       Control code (eg, \cC is ctrl-c)
  \OOO      Up to 3 octal digits -> byte value
  */
  char GetEscapeChar()
  { char c = ReadChar();
    if(char.IsDigit(c))
    { int num = (c-'0');
      for(int i=1; i<3; i++)
      { c = ReadChar();
        if(!char.IsDigit(c)) { lastChar=c; break; }
        num = num*10 + (c-'0');
      }
      if(num>255) throw SyntaxError("character value out of range");
      return (char)num;
    }
    else switch(c)
    { case '\"': return '\"';
      case '\'': return '\'';
      case 'n':  return '\n';
      case 't':  return '\t';
      case 'r':  return '\r';
      case 'b':  return '\b';
      case 'e':  return (char)27;
      case 'a':  return '\a';
      case 'f':  return '\f';
      case 'v':  return '\v';
      case '\\': return '\\';
      case 'x': case 'u':
      { int num = 0;
        for(int i=0,limit=(c=='x'?2:4); i<limit; i++)
        { c = ReadChar();
          if(char.IsDigit(c)) num = (num<<4) | (c-'0');
          else if((c<'A' || c>'F') || (c<'a' || c>'f'))
          { if(i==0) throw SyntaxError("expected hex digit");
            lastChar = c;
            break;
          }
          else num = (num<<4) | (char.ToUpper(c)-'A'+10);
        }
        return (char)num;
      }
      case 'c':
        c = char.ToUpper(ReadChar());
        if(c>96 || c<65) throw SyntaxError("expected control character, but received '{0}'", c);
        return (char)(c-64);
      default: throw SyntaxError("unknown escape character '{0}' (0x{1})", c, Ops.ToHex((uint)c, 2));
    }
  }

  Token NextToken()
  { if(nextToken!=Token.None)
    { token = nextToken;
      nextToken = Token.None;
    }
    else token = ReadToken();
    return token;
  }

  char ReadChar()
  { char c;
    if(lastChar!=0) { c=lastChar; lastChar='\0'; return c; }
    else if(pos>=data.Length) return '\0';
    c = data[pos++]; column++;
    if(c=='\n') { line++; column=0; }
    else if(c=='\r')
    { if(pos<data.Length && data[pos]=='\n') pos++;
      c='\n'; line++; column=0;
    }
    else if(c==0) c = ' ';
    return c;
  }

  object ReadNumber(char c)
  { StringBuilder sb = new StringBuilder();
    sb.Append(c);
    while(!IsDelimiter(c=ReadChar())) sb.Append(c);
    lastChar = c;

    if(sb.Length==1)
    { if(sb[0]=='.') return Token.Period;
      if(sb[0]=='-') return "-";
      if(sb[0]=='+') return "+";
    }

    string str = sb.ToString();
    int radix  = 10;
    char exact = '\0';
    bool hasFlags=false;

    if(str[0]=='#')
    { int start;
      for(start=1; start<str.Length; start++)
        switch(str[start])
        { case 'b': radix=2; break;
          case 'o': radix=8; break;
          case 'd': radix=10; break;
          case 'x': radix=16; break;
          case 'e': case 'i': exact=str[start]; break;
          default: goto done;
        }
      done:
      str = str.Substring(start);
      hasFlags = true;
    }

    Match m = (radix==10 ? decNum : radix==16 ? hexNum : radix==8 ? octNum : binNum).Match(str);
    if(!m.Success)
    { if(hasFlags) throw SyntaxError("invalid number: "+sb.ToString());
      else return str;
    }

    if(m.Groups["den"].Success)
    { if(exact!='i') throw new NotImplementedException("rationals");
      return Convert.ToDouble(ParseInt(m.Groups["num"].Value, radix)) /
             Convert.ToDouble(ParseInt(m.Groups["den"].Value, radix));
    }

    object num = ParseNum(m.Groups["num"].Value, m.Groups["exp"].Value, radix, exact);

    if(m.Groups["imag"].Success)
    { if(exact=='e') throw new NotImplementedException("exact complexes");
      return new Complex(Convert.ToDouble(num),
                         Convert.ToDouble(ParseNum(m.Groups["imag"].Value, m.Groups["imagexp"].Value, radix, exact)));
    }

    return num;
  }

  Token ReadToken()
  { char c;

    while(true)
    { endPos = new Position(line, column);
      do c=ReadChar(); while(c!=0 && char.IsWhiteSpace(c));
      currentPos = new Position(line, column);

      if(char.IsDigit(c) || c=='.' || c=='-' || c=='+')
      { value = ReadNumber(c);
        return value is Token ? (Token)value : value is string ? Token.Symbol : Token.Literal;
      }
      else if(c=='#')
      { c = ReadChar();
        switch(c)
        { case 't': value=true; return Token.Literal;
          case 'f': value=false; return Token.Literal;
          case '\\':
          { value = c = ReadChar();
            if(char.IsLetter(c))
            { StringBuilder sb = new StringBuilder();
              do
              { sb.Append(c);
                c = ReadChar();
              } while(c!=0 && !IsDelimiter(c));
              lastChar=c;

              try { value = Builtins.nameToChar.core(sb.ToString()); }
              catch(ValueErrorException e) { throw SyntaxError(e.Message); }
            }
            return Token.Literal;
          }
          case '%': case '_':
          { StringBuilder sb = new StringBuilder();
            sb.Append('#').Append(c);
            while(true)
            { c = ReadChar();
              if(IsDelimiter(c)) { lastChar=c; break; }
              sb.Append(c);
            }
            value = sb.ToString();
            return Token.Symbol;
          }
          case '"': case '\'':
          { char delim = c;
            StringBuilder sb = new StringBuilder();
            while(true)
            { c = ReadChar();
              if(c==delim)
              { c = ReadChar();
                if(c!=delim) { lastChar=c; break; }
              }
              else if(c==0) throw SyntaxError("unterminated string literal");
              else sb.Append(c);
            }
            value = sb.ToString();
            return Token.Literal;
          }
          case '(': return Token.Vector;
          case '|':
            while(true)
            { c = ReadChar();
              if(c=='|')
              { c = ReadChar();
                if(c=='#') break;
              }
              if(c==0) throw SyntaxError("unterminated extended comment");
            }
            break;
          case 'b': case 'o': case 'd': case 'x': case 'i': case 'e':
            lastChar=c; value=ReadNumber('#'); return Token.Literal;
          case '<': throw SyntaxError("unable to read: #<...");
          default: throw SyntaxError("unknown notation: #"+c);
        }
      }
      else if(c=='"')
      { StringBuilder sb = new StringBuilder();
        while(true)
        { c = ReadChar();
          if(c=='"') break;
          if(c==0) throw SyntaxError("unterminated string constant");
          if(c=='\\') c = GetEscapeChar();
          sb.Append(c);
        }
        value = sb.ToString();
        return Token.Literal;
      }
      else switch(c)
      { case '`': return Token.BackQuote;
        case ',':
        { c = ReadChar();
          if(c=='@') return Token.Splice;
          else { lastChar=c; return Token.Comma; }
        }
        case '(': return Token.LParen;
        case ')': return Token.RParen;
        case '[': return Token.LBracket;
        case ']': return Token.RBracket;
        case '{': return Token.LCurly;
        case '}': return Token.RCurly;
        case '\'': return Token.Quote;
        case '\0': return Token.EOF;
        case ';': while((c=ReadChar())!='\n' && c!=0); break;
        default:
        { StringBuilder sb = new StringBuilder();
          do
          { sb.Append(c);
            c = ReadChar();
          } while(c!=0 && !IsDelimiter(c));
          lastChar = c;
          string str = sb.ToString();
          if(str=="nil") { value=null; return Token.Literal; }
          else { value=str; return Token.Symbol; }
        }
      }
    }
  }

  SyntaxErrorException SyntaxError(string message)
  { return new SyntaxErrorException(string.Format("{0}({1},{2}): {3}", sourceFile, line, column, message));
  }
  SyntaxErrorException SyntaxError(string format, params object[] args)
  { return SyntaxError(string.Format(format, args));
  }

  bool TryEat(Token type)
  { if(token==type) { NextToken(); return true; }
    return false;
  }

  void Unexpected(Token token) { SyntaxError("unexpected token {0}", token, sourceFile, line, column); }
  void Unexpected(Token got, Token expect)
  { SyntaxError("unexpected token {0} (expecting {1})", got, expect, sourceFile, line, column);
  }

  string sourceFile, data;
  Token  token=Token.None, nextToken=Token.None;
  object value;
  Position currentPos=new Position(1,0), endPos=new Position(1,0);
  int    line=1, column=0, pos;
  char   lastChar;

  static object ParseInt(string str, int radix)
  { if(str=="") return 0;
    try { return Convert.ToInt32(str, radix); }
    catch(OverflowException)
    { try { return Convert.ToInt64(str, radix); }
      catch(OverflowException) { return new Integer(str, radix); }
    }
  }

  static double ParseNum(string str, int radix)
  { if(radix==10) return double.Parse(str);
    else
    { int pos = str.IndexOf('.');
      if(pos==-1) return Convert.ToDouble(ParseInt(str, radix));
      double whole=Convert.ToDouble(ParseInt(str.Substring(0, pos), radix));
      double  part=Convert.ToDouble(ParseInt(str.Substring(pos+1), radix));
      return whole + part/radix;
    }
  }

  static object ParseNum(string str, string exp, int radix, char exact)
  { double num = ParseNum(str, radix);
    if(exp!="") num *= Math.Pow(10, ParseNum(exp, radix));

    if(Math.IEEERemainder(num, 1)==0) // integer
    { if(exact=='i') return num;
      try { return checked((int)num); }
      catch(OverflowException)
      { try { return checked((long)num); }
        catch(OverflowException) { return new Integer(num); }
      }
    }
    else 
    { if(exact=='e') throw new NotImplementedException("rationals");
      return num;
    }
  }

  static readonly Symbol quoteSym=Symbol.Get("quote"), unquoteSym=Symbol.Get("unquote"),
                         spliceSym=Symbol.Get("unquote-splicing"), quasiSym=Symbol.Get("quasiquote"),
                         vectorSym=Symbol.Get("vector"), memberSym=Symbol.Get(".member"),
                         dotLastSym=Symbol.Get(".last");

  static readonly Regex binNum =
    new Regex(@"^(:?(?<num>[+-]?(?:[01]+(?:\.[01]*)?|\.[01]+))(?:e(?<exp>[+-]?(?:[01]+(?:\.[01]*)?|\.[01]+)))?
                   (?:(?<imag>[+-]?(?:[01]+(?:\.[01]*)?|\.[01]+))(?:e(?<imagexp>[+-]?(?:[01]+(?:\.[01]*)?|\.[01]+)))?i)?
                   |
                   (?<num>[+-]?[01]+)/(?<den>[+-]?[01]+)
                 )$",
              RegexOptions.Compiled|RegexOptions.IgnoreCase|RegexOptions.IgnorePatternWhitespace|RegexOptions.Singleline);

  static readonly Regex octNum =
    new Regex(@"^(?:(?<num>[+-]?(?:[0-7]+(?:\.[0-7]*)?|\.[0-7]+))(?:e(?<exp>[+-]?(?:[0-7]+(?:\.[0-7]*)?|\.[0-7]+)))?
                    (?:(?<imag>[+-]?(?:[0-7]+(?:\.[0-7]*)?|\.[0-7]+))(?:e(?<imagexp>[+-]?(?:[0-7]+(?:\.[0-7]*)?|\.[0-7]+)))?i)?
                    |
                    (?<num>[+-]?[0-7]+)/(?<den>[+-]?[0-7]+)
                 )$",
              RegexOptions.Compiled|RegexOptions.IgnoreCase|RegexOptions.IgnorePatternWhitespace|RegexOptions.Singleline);

  static readonly Regex decNum =
    new Regex(@"^(?:(?<num>[+-]?(?:\d+(?:\.\d*)?|\.\d+))(?:e(?<exp>[+-]?(?:\d+(?:\.\d*)?|\.\d+)))?
                    (?:(?<imag>[+-]?(?:\d+(?:\.\d*)?|\.\d+))(?:e(?<imagexp>[+-]?(?:\d+(?:\.\d*)?|\.\d+)))?i)?
                    |
                    (?<num>[+-]?\d+)/(?<den>[+-]?\d+)
                 )$",
              RegexOptions.Compiled|RegexOptions.IgnoreCase|RegexOptions.IgnorePatternWhitespace|RegexOptions.Singleline);

  static readonly Regex hexNum =
    new Regex(@"^(?:(?<num>[+-]?(?:[\da-f]+(?:\.[\da-f]*)?|\.[\da-f]+))(?:e(?<exp>[+-]?(?:[\da-f]+(?:\.[\da-f]*)?|\.[\da-f]+)))?
                    (?:(?<imag>[+-]?(?:[\da-f]+(?:\.[\da-f]*)?|\.[\da-f]+))(?:e(?<imagexp>[+-]?(?:[\da-f]+(?:\.[\da-f]*)?|\.[\da-f]+)))?i)?
                    |
                    (?<num>[+-]?[\da-f]+)/(?<den>[+-]?[\da-f]+)
                 )$",
              RegexOptions.Compiled|RegexOptions.IgnoreCase|RegexOptions.IgnorePatternWhitespace|RegexOptions.Singleline);
}

} // namespace NetLisp.Backend