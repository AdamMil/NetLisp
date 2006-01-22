/*
NetLisp is the reference implementation for a language similar to
Scheme, also called NetLisp. This implementation is both interpreted
and compiled, targetting the Microsoft .NET Framework.

http://www.adammil.net/
Copyright (C) 2005-2006 Adam Milazzo

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
using Scripting;

namespace NetLisp.Backend
{

#region LispOps
public sealed class LispOps
{ public static Pair Append(Pair list1, Pair list2)
  { if(list1==null) return list2;
    else
    { if(list2!=null) Mods.Srfi1.lastPair.core(list1).Cdr=list2;
      return list1;
    }
  }

  public static Snippet CompileRaw(object obj)
  { Language old = Options.Current.Language;
    try
    { Options.Current.Language = LispLanguage.Instance;
      return SnippetMaker.Generate(Scripting.AST.CreateCompiled(AST.Create(obj)));
    }
    finally { Options.Current.Language = old; }
  }

  public static Pair DottedList(object last, params object[] items)
  { Pair head=new Pair(items[0], null), tail=head;
    for(int i=1; i<items.Length; i++)
    { Pair next=new Pair(items[i], null);
      tail.Cdr = next;
      tail     = next;
    }
    tail.Cdr = last;
    return head;
  }

  public static bool EqualP(object a, object b)
  { if(a==b) return true;

    Pair pa=a as Pair;
    if(pa!=null)
    { Pair pb=b as Pair;
      if(pb!=null)
      { do
        { if(!EqualP(pa.Car, pb.Car)) return false;
          Pair next=pa.Cdr as Pair;
          if(next==null && pa.Cdr!=null) return EqualP(pa.Cdr, pb.Cdr);
          pa = next;
          pb = pb.Cdr as Pair;
        } while(pa!=null && pb!=null);
        return pa==pb;
      }
    }
    else if(!(b is Pair)) return Ops.AreEqual(a, b);
    return false;
  }

  public static Pair ExpectList(object obj)
  { if(obj==null) return null;
    Pair ret = obj as Pair;
    if(ret==null) throw new ArgumentException("expected list but received "+Ops.TypeName(obj));
    return ret;
  }

  public static Pair ExpectPair(object obj)
  { Pair ret = obj as Pair;
    if(ret==null) throw new ArgumentException("expected pair but received "+Ops.TypeName(obj));
    return ret;
  }

  public static Promise ExpectPromise(object obj)
  { Promise ret = obj as Promise;
    if(ret==null) throw new ArgumentException("expected promise but received "+Ops.TypeName(obj));
    return ret;
  }

  public static Symbol ExpectSymbol(object obj)
  { Symbol ret = obj as Symbol;
    if(ret==null) throw new ArgumentException("expected symbol but received "+Ops.TypeName(obj));
    return ret;
  }

  public static object FastCadr(Pair pair) { return ((Pair)pair.Cdr).Car; }
  public static object FastCddr(Pair pair) { return ((Pair)pair.Cdr).Cdr; }

  public void Import(MemberContainer mc, TopLevel top, Pair bindings)
  { if(bindings==null)
    { object obj;
      if(Ops.GetSlot(mc, "*EXPORTS*", out obj)) bindings = obj as Pair;
    }
    if(bindings==null) mc.Import(top, null, null);
    else
      using(CachedArray names=CachedArray.Alloc(), asNames=CachedArray.Alloc())
      { do
        { string name = bindings.Car as string, asName;
          if(name!=null) asName=name;
          else
          { Pair pair = bindings.Car as Pair;
            if(pair!=null)
            { name = pair.Car as string;
              asName = pair.Cdr as string;
            }
            else asName=null;
            if(name==null || asName==null)
              throw new ArgumentException("export list should be composed of strings and (name . asName) pairs");
          }
          names.Add(name);
          asNames.Add(asName);

          bindings = bindings.Cdr as Pair;
        } while(bindings!=null);

        mc.Import(top, (string[])names.ToArray(typeof(string)), (string[])asNames.ToArray(typeof(string)));
      }
  }

  public static Pair List(params object[] items) { return List(items, 0, items.Length); }
  public static Pair List(object[] items, int start) { return List(items, start, items.Length-start); }
  public static Pair List(object[] items, int start, int length)
  { Pair pair=null;
    for(int i=start+length-1; i>=start; i--) pair = new Pair(items[i], pair);
    return pair;
  }

  public static Pair List2(object first, params object[] items) { return new Pair(first, List(items)); }

  public static object[] ListToArray(Pair pair)
  { if(pair==null) return Ops.EmptyArray;
    using(CachedArray items = CachedArray.Alloc())
    { while(pair!=null) { items.Add(pair.Car); pair = pair.Cdr as Pair; }
      return (object[])items.ToArray(typeof(object));
    }
  }
}
#endregion

#region Pair
public sealed class Pair
{ public Pair(object car, object cdr) { Car=car; Cdr=cdr; }

  public override string ToString()
  { if(Mods.Srfi1.circularListP.core(this)) return "(!!)";

    System.Text.StringBuilder sb = new System.Text.StringBuilder();
    sb.Append('(');
    bool sep=false;

    Pair pair=this, next;
    do
    { if(sep) sb.Append(' ');
      else sep=true;
      sb.Append(Ops.ToCode(pair.Car));
      next = pair.Cdr as Pair;
      if(next==null)
      { if(pair.Cdr!=null) sb.Append(" . ").Append(Ops.ToCode(pair.Cdr));
        break;
      }
      else pair=next;
    } while(pair!=null);
    sb.Append(')');
    return sb.ToString();
  }

  public object Car, Cdr;
}
#endregion

#region Promise
public sealed class Promise
{ public Promise(IProcedure form) { Form = form; }
  public override string ToString() { return "#<promise>"; }
  public IProcedure Form;
  public object Value;
}
#endregion

#region Symbol
public sealed class Symbol
{ public Symbol(string name) { Name=name; }

  public readonly string Name;
  public override string ToString() { return Name; }

  public static Symbol Get(string name)
  { lock(table)
    { Symbol sym = (Symbol)table[name];
      if(sym==null) table[name] = sym = new Symbol(name);
      return sym;
    }
  }

  static readonly Hashtable table = new Hashtable();
}
#endregion

} // namespace NetLisp.Backend