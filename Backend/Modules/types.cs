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
using Scripting.Backend;
using NetLisp.Backend;

namespace NetLisp.Mods
{

public sealed class types
{ types() { }

  public static readonly ReflectedType @bool = ReflectedType.FromType(typeof(bool));
  public static readonly ReflectedType cast = ReflectedType.FromType(typeof(Cast));
  public static readonly ReflectedType @char = ReflectedType.FromType(typeof(char));
  public static readonly ReflectedType complex = ReflectedType.FromType(typeof(Complex));
  public static readonly ReflectedType enumerator = ReflectedType.FromType(typeof(IEnumerator));
  public static readonly ReflectedType fixnum32 = ReflectedType.FromType(typeof(int));
  public static readonly ReflectedType fixnum64 = ReflectedType.FromType(typeof(long));
  public static readonly ReflectedType float64 = ReflectedType.FromType(typeof(double));
  public static readonly ReflectedType integer = ReflectedType.FromType(typeof(Integer));
  public static readonly ReflectedType nil = ReflectedType.NullType;
  public static readonly ReflectedType @object = ReflectedType.FromType(typeof(object));
  public static readonly ReflectedType pair = ReflectedType.FromType(typeof(Pair));
  public static readonly ReflectedType promise = ReflectedType.FromType(typeof(Promise));
  public static readonly ReflectedType procedure = ReflectedType.FromType(typeof(IProcedure));
  public static readonly ReflectedType @ref = ReflectedType.FromType(typeof(Reference));
  public static readonly ReflectedType symbol = ReflectedType.FromType(typeof(Symbol));
  public static readonly ReflectedType @string = ReflectedType.FromType(typeof(string));
  public static readonly ReflectedType type = ReflectedType.FromType(typeof(ReflectedType));
  public static readonly ReflectedType values = ReflectedType.FromType(typeof(MultipleValues));
  public static readonly ReflectedType vector = ReflectedType.FromType(typeof(object[]));
}

} // namespace NetLisp.Mods