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
using System.Reflection;
using System.Reflection.Emit;
using Scripting;
using Scripting.Backend;

namespace NetLisp.Backend
{

public static class CG
{ public static void EmitList(CodeGenerator cg, Node[] items) { EmitList(cg, items, null, 0); }
  public static void EmitList(CodeGenerator cg, Node[] items, Node dot) { EmitList(cg, items, dot, 0); }
  public static void EmitList(CodeGenerator cg, Node[] items, int start) { EmitList(cg, items, null, start); }
  public static void EmitList(CodeGenerator cg, Node[] items, Node dot, int start)
  { bool hasTryNode = Node.HasExcept(items, start, items.Length-start);
    if(!hasTryNode) hasTryNode = dot!=null && dot.ClearsStack;

    ConstructorInfo cons = typeof(Pair).GetConstructor(new Type[] { typeof(object), typeof(object) });
    if(start==items.Length) cg.EmitNull();
    else if(!hasTryNode)
    { for(int i=start; i<items.Length; i++) items[i].Emit(cg);
      cg.EmitNode(dot);
      for(int i=start; i<items.Length; i++) cg.EmitNew(cons);
    }
    else if(start==items.Length-1)
    { if(dot==null)
      { items[start].Emit(cg);
        cg.EmitNull();
        cg.EmitNew(cons);
      }
      else
      { Slot tmp=cg.AllocLocalTemp(typeof(object)), dtmp=cg.AllocLocalTemp(typeof(object));
        items[start].Emit(cg);
        tmp.EmitSet(cg);
        dot.Emit(cg);
        dtmp.EmitSet(cg);
        tmp.EmitGet(cg);
        dtmp.EmitGet(cg);
        cg.EmitNew(cons);
        cg.FreeLocalTemp(tmp);
        cg.FreeLocalTemp(dtmp);
      }
    }
    else
    { Slot head=cg.AllocLocalTemp(typeof(Pair)), tail=cg.AllocLocalTemp(typeof(Pair)),
           next=cg.AllocLocalTemp(typeof(Pair));
      FieldInfo cdr = typeof(Pair).GetField("Cdr");

      items[start].Emit(cg);
      cg.EmitNull();
      cg.EmitNew(cons);
      cg.ILG.Emit(OpCodes.Dup);
      head.EmitSet(cg);
      tail.EmitSet(cg);

      for(int i=start+1; i<items.Length; i++)
      { items[i].Emit(cg);
        cg.EmitNull();
        cg.EmitNew(cons);
        next.EmitSet(cg);
        tail.EmitGet(cg);
        next.EmitGet(cg);
        cg.EmitFieldSet(cdr);
        next.EmitGet(cg);
        tail.EmitSet(cg);
      }

      head.EmitGet(cg);

      cg.FreeLocalTemp(head);
      cg.FreeLocalTemp(tail);
      cg.FreeLocalTemp(next);
    }
  }

  public static void EmitPair(CodeGenerator cg, Node node) { node.EmitTyped(cg, typeof(Pair)); }
}

}