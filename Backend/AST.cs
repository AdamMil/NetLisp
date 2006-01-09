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
using System.Reflection;
using System.Reflection.Emit;
using Scripting;

namespace NetLisp.Backend
{

#region LispCode
[AttributeUsage(AttributeTargets.Class|AttributeTargets.Struct, AllowMultiple=true)]
public class LispCodeAttribute : ScriptCodeAttribute
{ public LispCodeAttribute(string code) : base(code, LispLanguage.Instance) { }
}
#endregion

#region LispLanguage
public sealed class LispLanguage : Language
{ static LispLanguage()
  { ArrayList cfunc = new ArrayList(new string[] {
      "eq?", "eqv?", "equal?", "null?", "pair?", "char?", "symbol?", "string?", "procedure?", "vector?", "values",
      "not", "string-null?", "string-length", "vector-length", "car", "cdr", "promise?",
      "char-upcase", "char-downcase", "->string", "list" });

    string[] mapped = new string[]
    { "bitnot", "~", "bitand", "&", "bitor", "|", "bitxor", "^", "=", "==", "eq?", "===", "eqv?", "==",
      "expt", "**", "lshift", "<<", "rshift", ">>", "exptmod", "**%", "not", "!", "string-ref", "string[]",
      "vector-ref", "object[]", "vector-set!", "object[]="
    };
    string[] straight = new string[] { "+", "-", "*", "/", "//", "%", "!=", "<", "<=", ">", ">=" };

    ops = new SortedList(mapped.Length/2+straight.Length);
    for(int i=0; i<mapped.Length; i+=2)
    { ops[mapped[i]] = mapped[i+1];
      cfunc.Add(mapped[i]);
    }
    foreach(string s in straight)
    { ops[s] = s;
      cfunc.Add(s);
    }

    cfunc.Sort();
    constant = (string[])cfunc.ToArray(typeof(string));
  }

  public override MemberContainer Builtins { get { return Backend.Builtins.Instance; }}
  public override string BuiltinsNamespace { get { return "NetLisp.Mods"; } }
  public override string Name { get { return "NetLisp"; } }

  #region EmitConstant
  public override bool EmitConstant(Scripting.CodeGenerator cg, object value)
  { if(value is Symbol)
    { Symbol sym = (Symbol)value;
      cg.EmitString(sym.Name);
      cg.EmitCall(typeof(Symbol), "Get");
    }
    else if(value is Pair)
    { Pair pair = (Pair)value;
      int count = 1;
      while(true)
      { cg.EmitConstantObject(pair.Car);
        Pair next = pair.Cdr as Pair;
        if(next==null) { cg.EmitConstantObject(pair.Cdr); break; }
        else { pair=next; count++; }
      }
      ConstructorInfo ci = typeof(Pair).GetConstructor(new Type[] { typeof(object), typeof(object) });
      for(int i=0; i<count; i++) cg.EmitNew(ci);
    }
    else return false;
    return true;
  }
  #endregion

  #region EvaluateConstantFunction
  public override bool EvaluateConstantFunction(string name, Node[] args, out object result)
  { object op = ops[name];
    if(op!=null) name = (string)op;
    else
    { object[] a = Node.MakeObjectArray(args);
      switch(name)
      { case "car":
          Ops.CheckArity(name, a, 1);
          Ops.CheckType(name, a, 0, typeof(Pair));
          result = ((Pair)a[0]).Car;
          break;
        case "cdr":
          Ops.CheckArity(name, a, 1);
          Ops.CheckType(name, a, 0, typeof(Pair));
          result = ((Pair)a[0]).Cdr;
          break;
        case "char?": Ops.CheckArity(name, a, 1); result = a[0] is char; break;
        case "char-downcase":
          Ops.CheckArity(name, a, 1);
          Ops.CheckType(name, a, 0, typeof(char));
          result = char.ToLower((char)a[0]);
          break;
        case "char-upcase":
          Ops.CheckArity(name, a, 1);
          Ops.CheckType(name, a, 0, typeof(char));
          result = char.ToUpper((char)a[0]);
          break;
        case "equal?": Ops.CheckArity(name, a, 2); result = LispOps.EqualP(a[0], a[1]); break;
        case "list": result = LispOps.List(a); break;
        case "pair?": Ops.CheckArity(name, a, 1); result = a[0] is Pair; break;
        case "promise?": Ops.CheckArity(name, a, 1); result = a[0] is Promise; break;
        case "null?": Ops.CheckArity(name, a, 1); result = a[0]==null; break;
        case "procedure?": Ops.CheckArity(name, a, 1); result = a[0] is IProcedure; break;
        case "string?": Ops.CheckArity(name, a, 1); result = a[0] is string; break;
        case "string-length":
          Ops.CheckArity(name, a, 1);
          Ops.CheckType(name, a, 0, typeof(string));
          result = ((string)a[0]).Length;
          break;
        case "string-null?": Ops.CheckArity(name, a, 1); result = a[0] is string && (string)a[0]==""; break;
        case "symbol?": Ops.CheckArity(name, a, 1); result = a[0] is Symbol; break;
        case "values": Ops.CheckArity(name, a, 1, -1); result = a.Length==1 ? a[0] : new MultipleValues(a); break;
        case "vector?": Ops.CheckArity(name, a, 1); result = a[0] is object[]; break;
        case "vector-length":
          Ops.CheckArity(name, a, 1);
          Ops.CheckType(name, a, 0, typeof(object[]));
          result = ((object[])a[0]).Length;
          break;
        default: goto tryBase;
      }
      return true;
    }

    tryBase: return base.EvaluateConstantFunction(name, args, out result);
  }
  #endregion

  public override bool ExcludeFromImport(string name) { return name.StartsWith("#_"); }

  public override Type GetInlinedResultType(string functionName)
  { string op = (string)ops[functionName];
    if(op!=null) return base.GetInlinedResultType(op);

    switch(functionName)
    { case "equal?": case "null?": case "pair?": case "char?": case "symbol?": case "string?": case "procedure?":
      case "vector?": case "promise?": case "string-null?":
        return typeof(bool);
      case "list": return typeof(Pair);
      case "char-upcase": case "char-downcase": return typeof(char);
      case "string-length": case "vector-length": return typeof(int);
      case "#%delay": return typeof(Promise);
      default: return typeof(object);
    }
  }

  #region InlineFunction
  public override bool InlineFunction(Scripting.CodeGenerator scg, string name, CallNode node, ref Type etype)
  { object op = ops[name];
    if(op!=null) return base.InlineFunction(scg, (string)op, node, ref etype);

    CodeGenerator cg = (CodeGenerator)scg;
    Node[] args = node.GetArgNodes();
    switch(name) // functions with side effects
    { case "set-car!": case "set-cdr!":
        node.CheckArity(2);
        if(etype==typeof(void))
        { cg.EmitPair(args[0]);
          args[1].Emit(cg);
          cg.EmitFieldSet(typeof(Pair), name=="set-car!" ? "Car" : "Cdr");
        }
        else
        { Slot tmp = cg.AllocLocalTemp(typeof(object));
          args[1].Emit(cg);
          tmp.EmitSet(cg);
          cg.EmitPair(args[0]);
          tmp.EmitGet(cg);
          cg.EmitFieldSet(typeof(Pair), name=="set-car!" ? "Car" : "Cdr");
          tmp.EmitGet(cg);
          cg.FreeLocalTemp(tmp);
          goto objret;
        }
        return true;
    }

    switch(name)
    { case "equal?":
      { node.CheckArity(2);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        args[0].Emit(cg);
        args[1].Emit(cg);
        cg.EmitCall(typeof(LispOps), "EqualP");
        if(etype!=typeof(bool))
        { cg.BoolToObject();
          goto objret;
        }
        return true;
      }
      case "null?": case "pair?": case "char?": case "symbol?": case "string?": case "procedure?": case "vector?":
      case "promise?": case "string-null?":
      { node.CheckArity(1);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        Type type=null;
        if(name=="string-null?") cg.EmitString(args[0]);
        else args[0].Emit(cg);
        switch(name)
        { case "pair?": type=typeof(Pair); break;
          case "char?": type=typeof(char); break;
          case "symbol?": type=typeof(Symbol); break;
          case "string?": type=typeof(string); break;
          case "procedure?": type=typeof(IProcedure); break;
          case "vector?": type=typeof(object[]); break;
          case "promise?": type=typeof(Promise); break;
          case "not": cg.EmitIsTrue(); break;
          case "string-null?": cg.EmitPropGet(typeof(string), "Length"); break;
        }
        if(etype==typeof(bool))
        { if(type!=null) cg.ILG.Emit(OpCodes.Isinst, type);
          else etype=typeof(CodeGenerator.negbool); // for 'not', above
        }
        else
        { if(type!=null) cg.ILG.Emit(OpCodes.Isinst, type);
          cg.BoolToObject(type==null);
          goto objret;
        }
        return true;
      }
      case "string-length": case "vector-length":
        node.CheckArity(1);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        if(name=="string-length")
        { cg.EmitString(args[0]);
          cg.EmitPropGet(typeof(string), "Length");
        }
        else
        { cg.EmitTypedNode(args[0], typeof(object[]));
          cg.EmitPropGet(typeof(object[]), "Length");
        }
        if(etype!=typeof(int))
        { cg.ILG.Emit(OpCodes.Box, typeof(int));
          goto objret;
        }
        return true;
      case "car": case "cdr":
        node.CheckArity(1);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        cg.EmitPair(args[0]);
        cg.EmitFieldGet(typeof(Pair), name=="car" ? "Car" : "Cdr");
        goto objret;
      case "char-upcase": case "char-downcase":
        node.CheckArity(1);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        cg.EmitTypedNode(args[0], typeof(char));
        cg.EmitCall(typeof(char), name=="char-upcase" ? "ToUpper" : "ToLower", typeof(char));
        if(etype!=typeof(char))
        { cg.ILG.Emit(OpCodes.Box, typeof(char));
          goto objret;
        }
        return true;
      case "#%delay":
        node.CheckArity(1);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        cg.EmitTypedNode(args[0], typeof(IProcedure));
        cg.EmitNew(typeof(Promise), typeof(IProcedure));
        etype = typeof(Promise);
        return true;
      case "list":
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        else
        { cg.EmitList(args);
          etype = typeof(Pair);
          return true;
        }
      case "values":
        node.CheckArity(1, -1);
        if(etype==typeof(void)) { cg.EmitVoids(args); return true; }
        if(args.Length==1) { cg.EmitNode(args[0]); goto objret; }
        else
        { cg.EmitObjectArray(args);
          cg.EmitNew(typeof(MultipleValues), typeof(object[]));
          etype = typeof(MultipleValues);
        }
        return true;
      default: return false;
    }

    objret:
    etype = typeof(object);
    return true;
  }
  #endregion

  public override bool IsConstantFunction(string name) { return Array.BinarySearch(constant, name)>=0; }
  public override bool IsHashableConstant(object value) { return value is Symbol; }

  public override Scripting.CodeGenerator MakeCodeGenerator(TypeGenerator tg, MethodBase mb, ILGenerator ilg)
  { return new CodeGenerator(tg, mb, ilg);
  }

  public override object PackArguments(object[] args, int start, int length)
  { return length>1 ? LispOps.List(args, start, length) : length==0 ? null : new Pair(args[start], null);
  }

  public override Node Parse(string sourceName, string code) { return Parse(new Parser(sourceName, code)); }
  public override Node Parse(string sourceName, System.IO.TextReader data)
  { return Parse(new Parser(sourceName, data));
  }

  #region Repr
  public override string Repr(object obj)
  { switch(Convert.GetTypeCode(obj))
    { case TypeCode.Boolean: return (bool)obj ? "#t" : "#f";
      case TypeCode.Char: return Backend.Builtins.charToName.core((char)obj, true);
      case TypeCode.Double: return ((double)obj).ToString("R");
      case TypeCode.Empty: return "nil";
      case TypeCode.Object:
        if(obj is object[])
        { System.Text.StringBuilder sb = new System.Text.StringBuilder();
          sb.Append("#(");
          bool sep=false;
          foreach(object o in (object[])obj)
          { if(sep) sb.Append(' ');
            else sep=true;
            sb.Append(Repr(o));
          }
          sb.Append(')');
          return sb.ToString();
        }
        break;
      case TypeCode.Single: return ((float)obj).ToString("R");
      case TypeCode.String:
      { string str = (string)obj;
        System.Text.StringBuilder sb = new System.Text.StringBuilder(str.Length+16);
        sb.Append('"');
        for(int i=0; i<str.Length; i++)
        { char c = str[i];
          if(c>=32 && c!='"' && c!='\\' && c!=127) sb.Append(c);
          else
            switch(c)
            { case '\n': sb.Append("\\n"); break;
              case '\r': sb.Append("\\r"); break;
              case '\"': sb.Append("\\\""); break;
              case '\\': sb.Append("\\\\"); break;
              case '\t': sb.Append("\\t"); break;
              case '\b': sb.Append("\\b"); break;
              case (char)27: sb.Append("\\e"); break;
              default:
                sb.Append(c==0 ? "\\0" : c<27 ? "\\c"+((char)(c+64)).ToString()
                                              : (c<256 ? "\\x" : "\\u")+Ops.ToHex((uint)c, c<256 ? 2 : 4));
                break;
            }
        }
        sb.Append('"');
        return sb.ToString();
      }
    }
    return obj.ToString();
  }
  #endregion

  public override string Repr(Node node) { throw new NotImplementedException("node repr"); }

  public override bool ShouldAddBuiltins(Type type) { return type!=typeof(Builtins); }

  #region TypeName
  public override string TypeName(Type type)
  { switch(Type.GetTypeCode(type))
    { case TypeCode.Boolean: return "bool";
      case TypeCode.Empty: return "nil";
      case TypeCode.Byte:  case TypeCode.SByte: return "fixnum8";
      case TypeCode.Int16: case TypeCode.UInt16: return "fixnum16";
      case TypeCode.Int32: case TypeCode.UInt32: return "fixnum32";
      case TypeCode.Int64: case TypeCode.UInt64: return "fixnum64";
      case TypeCode.Char: return "char";
      case TypeCode.Object:
        if(type==typeof(Symbol)) return "symbol";
        if(type==typeof(Pair)) return "pair";
        if(type==typeof(object[])) return "vector";
        if(type==typeof(IProcedure)) return "procedure";
        if(type==typeof(Integer)) return "bigint";
        if(type==typeof(Complex)) return "complex";
        if(type==typeof(MultipleValues)) return "multiplevalues";
        if(type==typeof(Reference)) return "ref";
        if(type==typeof(Promise)) return "promise";
        if(type==typeof(Type) || type==typeof(ReflectedType)) return "type";
        goto default;
      case TypeCode.Double: return "flonum64";
      case TypeCode.Single: return "flonum32";
      case TypeCode.String: return "string";
      default: return type.FullName;
    }
  }
  #endregion

  public static readonly LispLanguage Instance = new LispLanguage();

  static Node Parse(Parser parser)
  { object expand=null, data=parser.Parse();
    Ops.GetGlobal("expand", out expand);
    return AST.Create(expand==null ? data : Ops.ExpectProcedure(expand).Call(data));
  }

  static readonly SortedList ops;
  static readonly string[] constant;
}
#endregion

#region AST
public sealed class AST
{ public static Node Create(object obj)
  { Node body = Parse(obj);
    if(Options.Current.Debug)
    { SyntaxObject syntax = obj as SyntaxObject;
      if(syntax!=null) body = new MarkSourceNode(syntax.File, syntax.Code, body);
    }
    return body;
  }

  static Node Parse(object obj)
  { Symbol sym = obj as Symbol;
    SyntaxObject syntax = null;
    if(sym!=null)
    { string name = sym.Name;
      if(name==".last") return new LastNode();
      int pos = name.IndexOf('.', 1);
      Node var = new VariableNode(pos==-1 ? name : name.Substring(0, pos));
      return pos==-1 ? var : SetPos(syntax, new MemberNode(var, new LiteralNode(name.Substring(pos+1))));
    }

    Pair pair = obj as Pair;
    if(pair==null) return new LiteralNode(obj);

    sym = pair.Car as Symbol;
    if(sym!=null)
      switch(sym.Name)
      { case "quote":
        { if(Builtins.length.core(pair)!=2) throw Ops.SyntaxError("quote: expects exactly 1 form");
          return SetPos(syntax, Quote(LispOps.FastCadr(pair)));
        }
        case "if":
        { int len = Builtins.length.core(pair);
          if(len<3 || len>4) throw Ops.SyntaxError("if: expects 2 or 3 forms");
          pair = (Pair)pair.Cdr;
          Pair next = (Pair)pair.Cdr;
          return SetPos(syntax, new IfNode(Parse(pair.Car), Parse(next.Car),
                                           next.Cdr==null ? null : Parse(LispOps.FastCadr(next))));
        }
        case "let":
        { if(Builtins.length.core(pair)<3) goto error;
          pair = (Pair)pair.Cdr;

          Pair bindings = pair.Car as Pair;
          if(bindings==null) goto error;
          string[] names = new string[Builtins.length.core(bindings)];
          Node[]   inits = new Node[names.Length];
          for(int i=0; i<names.Length; bindings=(Pair)bindings.Cdr,i++)
          { if(bindings.Car is Pair)
            { Pair binding = (Pair)bindings.Car;
              sym = binding.Car as Symbol;
              inits[i] = Parse(LispOps.FastCadr(binding));
            }
            else sym = bindings.Car as Symbol;
            if(sym==null) goto error;
            names[i] = sym.Name;
          }

          return SetPos(syntax, new LocalBindNode(names, inits, ParseBody((Pair)pair.Cdr)));
          error: throw Ops.SyntaxError("let: must be of the form (let ([symbol | (symbol form)] ...) forms ...)");
        }
        case "begin":
        { if(Builtins.length.core(pair)<2) throw Ops.SyntaxError("begin: no forms given");
          return ParseBody((Pair)pair.Cdr);
        }
        case "lambda":
        { if(Builtins.length.core(pair)<3) throw Ops.SyntaxError("lambda: must be of the form (lambda bindings forms ...)");
          pair = (Pair)pair.Cdr;
          bool hasList;
          return SetPos(syntax, new LambdaNode(ParseLambdaList(pair.Car, out hasList), hasList, false,
                                               ParseBody((Pair)pair.Cdr)));
        }
        case "set!":
        { if(Builtins.length.core(pair)<3) goto error;
          ArrayList names=new ArrayList(), values=new ArrayList();
          pair = (Pair)pair.Cdr;
          do
          { sym = pair.Car as Symbol;
            if(sym==null) goto error;
            names.Add(new Name(sym.Name));
            pair = pair.Cdr as Pair;
            if(pair==null) goto error;
            values.Add(Parse(pair.Car));
            pair = pair.Cdr as Pair;
          } while(pair!=null);

          Node ret;
          if(names.Count==1) ret = new SetNode(new VariableNode((Name)names[0]), (Node)values[0]);
          else
          { Node[] sets = new Node[names.Count];
            for(int i=0; i<names.Count; i++) sets[i] = new SetNode(new VariableNode((Name)names[i]), (Node)values[i]);
            ret = new BodyNode(sets);
          }
          return SetPos(syntax, ret);
          error: throw Ops.SyntaxError("set!: must be of form (set! symbol form [symbol form] ...)");
        }
        case "define":
        { int length = Builtins.length.core(pair);
          if(length!=3) goto error;
          pair = (Pair)pair.Cdr;
          sym = pair.Car as Symbol;
          if(sym==null) goto error;
          return SetPos(syntax, new DefineNode(sym.Name, Parse(LispOps.FastCadr(pair))));
          error: throw Ops.SyntaxError("define: must be of form (define name value)");
        }
        case "vector": return SetPos(syntax, new VectorNode(ParseNodeList(pair.Cdr as Pair)));
        /* (let-values (((a b c) (values 1 2 3))
                        ((x y) (values 4 5 6)))
              (+ a b c x y))
        */
        case "values":
          if(Options.Current.OptimizeAny) return SetPos(syntax, new ValuesNode(ParseNodeList(pair.Cdr as Pair)));
          break;
        case "let-values":
        { if(Builtins.length.core(pair)<3) goto error;
          pair = (Pair)pair.Cdr;
          Pair bindings = pair.Car as Pair;
          if(bindings==null)
          { if(pair.Car==null) return ParseBody((Pair)pair.Cdr);
            goto error;
          }
          ArrayList names=new ArrayList(), inits=new ArrayList();
          do
          { Pair binding = bindings.Car as Pair;
            if(binding==null) goto bindingError;
            Pair namePair = binding.Car as Pair;
            if(namePair==null) goto bindingError;
            Name[] narr = new Name[Builtins.length.core(namePair)];
            for(int i=0; i<narr.Length; namePair=namePair.Cdr as Pair,i++)
            { sym = namePair.Car as Symbol;
              if(sym==null) goto bindingError;
              narr[i] = new Name(sym.Name);
            }
            names.Add(narr);
            inits.Add(Parse(LispOps.FastCadr(binding)));
            bindings = bindings.Cdr as Pair;
          } while(bindings!=null);

          return new ValueBindNode((Name[][])names.ToArray(typeof(Name[])),
                                   (Node[])inits.ToArray(typeof(Node)), ParseBody((Pair)pair.Cdr));
          error: throw Ops.SyntaxError("let-value: must be of form (let-values bindings form ...)");
          bindingError: throw Ops.SyntaxError("let-value: bindings must be of form (((symbol ...) form) ...)");
        }
        /* (try
            (begin form...)
            (catch (e type...)
              form...)
            (catch () form...)
            (finally form...))
        */
        case "try":
        { if(Builtins.length.core(pair)<3) goto error;
          pair = (Pair)pair.Cdr;
          Node final=null, body=Parse(pair.Car);
          ArrayList excepts=null, etypes=null;

          while((pair=(Pair)pair.Cdr) != null)
          { Pair form = pair.Car as Pair;
            if(form==null) goto error;
            sym = form.Car as Symbol;
            if(sym==null) goto error;
            if(sym.Name=="catch")
            { if(excepts==null) excepts=new ArrayList();
              string evar;
              if(Builtins.length.core(form)<3) goto catchError;
              form = (Pair)form.Cdr;
              if(form.Car is Pair)
              { Pair epair = (Pair)form.Car;
                if(Builtins.length.core(epair)>2) goto catchError;
                sym = epair.Car as Symbol;
                if(sym==null) goto catchError;
                evar = sym.Name;
                epair = (Pair)epair.Cdr;
                if(epair!=null)
                { if(etypes==null) etypes = new ArrayList();
                  do etypes.Add(Parse(epair.Car)); while((epair=(Pair)epair.Cdr) != null);
                }
              }
              else if(form.Car!=null) goto catchError;
              else evar = null;
              excepts.Add(new Except(evar, etypes==null ? null : (Node[])etypes.ToArray(typeof(Node)),
                                     ParseBody((Pair)form.Cdr)));
              if(etypes!=null) etypes.Clear();
            }
            else if(sym.Name=="finally")
            { if(final!=null) goto error;
              final = ParseBody((Pair)form.Cdr);
            }
            else goto error;
          }

          if(excepts==null && final==null) goto error;
          return new TryNode(body, final, excepts==null ? null : (Except[])excepts.ToArray(typeof(Except)));
          error: throw Ops.SyntaxError("try form expects one body form followed by catch forms and/or an optional finally form");
          catchError: throw Ops.SyntaxError("catch form should be of the form: (catch ([e [type ...]]) forms...)");
        }
        // (throw [type [objects ...]]) ; type-less throw only allowed within catch form
        case "throw":
        { Node type = null;
          Node[] objs = null;

          pair = pair.Cdr as Pair;
          if(pair!=null)
          { type = Parse(pair.Car);
            objs = ParseNodeList(pair.Cdr as Pair);
          }

          return SetPos(syntax, new ThrowNode(type, objs));
        }
        // (.member object member-name)
        case ".member":
        { int length = Builtins.length.core(pair);
          if(length!=3) throw Ops.SyntaxError(".member: must be of form (.member obj-form name-form)");
          pair = (Pair)pair.Cdr;
          return SetPos(syntax, new MemberNode(Parse(pair.Car), Parse(LispOps.FastCadr(pair))));
        }
      }

    Node func = Parse(pair.Car);
    Node[] args = ParseNodeList(pair.Cdr as Pair);
    // TODO: move this optimization into Scripting
    if(Options.Current.OptimizeAny && func is LambdaNode) // optimization: transform ((lambda (a) ...) x) into (let ((a x)) ...)
    { LambdaNode fl = (LambdaNode)func;
      string[] names = new string[fl.Parameters.Length];
      for(int i=0; i<names.Length; i++) names[i] = fl.Parameters[i].Name.String;
      int positional = fl.Parameters.Length-(fl.HasList ? 1 : 0);
      Ops.CheckArity("<unnamed lambda>", args.Length, positional, fl.HasList ? -1 : positional);

      Node[] inits = new Node[names.Length];
      for(int i=0; i<positional; i++) inits[i] = args[i];
      if(fl.HasList)
      { if(args.Length==positional) inits[positional] = new LiteralNode(null);
        else
        { Node[] elems = new Node[args.Length-positional];
          for(int i=positional; i<args.Length; i++) elems[i-positional] = args[i];
          inits[positional] = new ListNode(elems, null);
        }
      }
      return new LocalBindNode(names, inits, fl.Body);
    }
    else return SetPos(syntax, new CallNode(func, args));
  }

  static Node ParseBody(Pair start)
  { Node[] list = ParseNodeList(start);
    return list==null ? null : list.Length==1 ? list[0] : new BodyNode(list);
  }

  static string[] ParseLambdaList(object obj, out bool hasList)
  { hasList = false;
    Symbol sym = obj as Symbol;
    if(sym!=null) { hasList=true; return new string[] { sym.Name }; }

    Pair list = (Pair)obj;
    ArrayList names = new ArrayList();
    while(list!=null)
    { sym = list.Car as Symbol;
      if(sym==null) goto error;
      names.Add(sym.Name);
      object next = list.Cdr;
      list = next as Pair;
      if(list==null && next!=null)
      { sym = next as Symbol;
        if(sym==null) goto error;
        names.Add(sym.Name);
        hasList=true;
        break;
      }
    }
    return (string[])names.ToArray(typeof(string));

    error: throw Ops.SyntaxError("lambda bindings must be of the form: symbol | (symbol... [ . symbol])");
  }

  static Node[] ParseNodeList(Pair start)
  { if(start==null) return null;
    ArrayList items = new ArrayList();
    while(start!=null)
    { items.Add(Parse(start.Car));
      start = start.Cdr as Pair;
    }
    return (Node[])items.ToArray(typeof(Node));
  }

  static Node Quote(object obj)
  { Pair pair = obj as Pair;
    if(pair==null) return new LiteralNode(obj);

    ArrayList items = new ArrayList();
    Node dot=null;
    while(true)
    { items.Add(Quote(pair.Car));
      object next = pair.Cdr;
      pair = next as Pair;
      if(pair==null)
      { if(next!=null) dot = Quote(next);
        break;
      }
    }
    return new ListNode((Node[])items.ToArray(typeof(Node)), dot);
  }

  static Node SetPos(SyntaxObject syntax, Node node)
  { if(syntax!=null)
    { node.StartPos = syntax.Start;
      node.EndPos   = syntax.End;
    }
    return node;
  }
}
#endregion

#region DefineNode
public sealed class DefineNode : Node
{ public DefineNode(string name, Node value) { Set = new SetNode(name, value, SetType.Set); }

  public override void Emit(Scripting.CodeGenerator cg, ref Type etype)
  { Set.EmitVoid(cg);
    if(etype!=typeof(void))
    { cg.EmitConstantObject(Symbol.Get(((VariableNode)Set.LHS[0]).Name.String));
      etype = typeof(Symbol);
    }
    TailReturn(cg);
  }

  public override object Evaluate()
  { Set.Evaluate();
    return Symbol.Get(((VariableNode)Set.LHS[0]).Name.String);
  }

  public override Type GetNodeType() { return typeof(Symbol); }

  public override void MarkTail(bool tail) { Tail=tail; Set.MarkTail(false); }

  public override void Walk(IWalker w)
  { if(w.Walk(this)) Set.Walk(w);
    w.PostWalk(this);
  }

  public readonly SetNode Set;
}
#endregion

#region LastNode
public sealed class LastNode : Node
{ public override void Emit(Scripting.CodeGenerator cg, ref Type etype)
  { if(etype!=typeof(void))
    { cg.EmitFieldGet(typeof(LispOps), "LastPtr");
      etype = typeof(object);
      TailReturn(cg);
    }
  }

  public override object Evaluate() { return LispOps.LastPtr; }
  public override Type GetNodeType() { return typeof(object); }
  public override void Walk(IWalker w) { if(w.Walk(this)) w.PostWalk(this); }
}
#endregion

#region ListNode
public sealed class ListNode : Node
{ public ListNode(Node[] items, Node dot) { Items=items; Dot=dot; }

  public override void Emit(Scripting.CodeGenerator cg, ref Type etype)
  { if(!IsConstant || etype!=typeof(void))
    { if(IsConstant) cg.EmitConstantObject(Evaluate());
      else
      { cg.MarkPosition(this);
        ((CodeGenerator)cg).EmitList(Items, Dot);
      }
      etype = Items.Length==0 && Dot==null ? null : typeof(Pair);
    }
    TailReturn(cg);
  }

  public override object Evaluate()
  { object obj = Dot==null ? null : Dot.Evaluate();
    for(int i=Items.Length-1; i>=0; i--) obj = new Pair(Items[i].Evaluate(), obj);
    return obj;
  }

  public override Type GetNodeType() { return Items.Length==0 ? null : typeof(Pair); }

  public override void Optimize()
  { bool isconst=(Dot==null || Dot.IsConstant);
    if(isconst) for(int i=0; i<Items.Length; i++) if(!Items[i].IsConstant) { isconst=false; break; }
    IsConstant = isconst;
  }

  public override void Walk(IWalker w)
  { if(w.Walk(this))
    { foreach(Node n in Items) n.Walk(w);
      if(Dot!=null) Dot.Walk(w);
    }
    w.PostWalk(this);
  }

  public readonly Node[] Items;
  public readonly Node Dot;
}
#endregion

#region MemberNode
public sealed class MemberNode : Scripting.MemberNode
{ public MemberNode(Node value, Node members) : base(value, members) { }

  protected override void HandleThisPtr(Scripting.CodeGenerator cg)
  { cg.ILG.Emit(OpCodes.Dup);
    cg.EmitFieldSet(typeof(LispOps), "LastPtr");
  }

  protected override void HandleThisPtr(Scripting.CodeGenerator cg, Slot slot)
  { slot.EmitGet(cg);
    cg.EmitFieldSet(typeof(LispOps), "LastPtr");
  }
}
#endregion

} // namespace NetLisp.Backend