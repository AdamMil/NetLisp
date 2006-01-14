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
using System.Reflection;
using System.Reflection.Emit;
using Scripting;

namespace NetLisp.Backend
{

public sealed class CodeGenerator : Scripting.CodeGenerator
{ public CodeGenerator(TypeGenerator tg, MethodBase mb, ILGenerator ilg) : base(tg, mb, ilg) { }

  public void EmitList(Node[] items) { EmitList(items, null, 0); }
  public void EmitList(Node[] items, Node dot) { EmitList(items, dot, 0); }
  public void EmitList(Node[] items, int start) { EmitList(items, null, start); }
  public void EmitList(Node[] items, Node dot, int start)
  { bool hasTryNode = false;
    for(int i=start; i<items.Length; i++) if(items[i] is TryNode) { hasTryNode=true; break; }
    if(!hasTryNode) hasTryNode = dot is TryNode;

    ConstructorInfo cons = typeof(Pair).GetConstructor(new Type[] { typeof(object), typeof(object) });
    if(start==items.Length) ILG.Emit(OpCodes.Ldnull);
    else if(!hasTryNode)
    { for(int i=start; i<items.Length; i++) items[i].Emit(this);
      EmitNode(dot);
      for(int i=start; i<items.Length; i++) EmitNew(cons);
    }
    else if(start==items.Length-1)
    { if(dot==null)
      { items[start].Emit(this);
        ILG.Emit(OpCodes.Ldnull);
        EmitNew(cons);
      }
      else
      { Slot tmp=AllocLocalTemp(typeof(object)), dtmp=AllocLocalTemp(typeof(object));
        items[start].Emit(this);
        tmp.EmitSet(this);
        dot.Emit(this);
        dtmp.EmitSet(this);
        tmp.EmitGet(this);
        dtmp.EmitGet(this);
        EmitNew(cons);
        FreeLocalTemp(tmp);
        FreeLocalTemp(dtmp);
      }
    }
    else
    { Slot head=AllocLocalTemp(typeof(Pair)), tail=AllocLocalTemp(typeof(Pair)), next=AllocLocalTemp(typeof(Pair));
      FieldInfo cdr = typeof(Pair).GetField("Cdr");

      items[start].Emit(this);
      ILG.Emit(OpCodes.Ldnull);
      EmitNew(cons);
      ILG.Emit(OpCodes.Dup);
      head.EmitSet(this);
      tail.EmitSet(this);

      for(int i=start+1; i<items.Length; i++)
      { items[i].Emit(this);
        ILG.Emit(OpCodes.Ldnull);
        EmitNew(cons);
        next.EmitSet(this);
        tail.EmitGet(this);
        next.EmitGet(this);
        EmitFieldSet(cdr);
        next.EmitGet(this);
        tail.EmitSet(this);
      }

      head.EmitGet(this);

      FreeLocalTemp(head);
      FreeLocalTemp(tail);
      FreeLocalTemp(next);
    }
  }

  public void EmitPair(Node node) { node.EmitTyped(this, typeof(Pair)); }
}

}