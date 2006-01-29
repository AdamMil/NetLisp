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
using Scripting;

namespace NetLisp.Mods
{

public static class math
{ // TODO: lcm, gcd, etc
  #region abs
  public sealed class abs : Primitive
  { public abs() : base("abs", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      switch(Convert.GetTypeCode(obj))
      { case TypeCode.Byte: case TypeCode.UInt16: case TypeCode.UInt32: case TypeCode.UInt64: return obj;
        case TypeCode.Decimal: return Math.Abs((Decimal)obj);
        case TypeCode.Double: return Math.Abs((double)obj);
        case TypeCode.Int16: return Math.Abs((short)obj);
        case TypeCode.Int32: return Math.Abs((int)obj);
        case TypeCode.Int64: return Math.Abs((long)obj);
        case TypeCode.SByte: return Math.Abs((sbyte)obj);
        case TypeCode.Single: return Math.Abs((float)obj);
        case TypeCode.Object:
          if(obj is Integer) return ((Integer)obj).Abs;
          if(obj is Complex)
          { Complex c = (Complex)obj;
            if(c.imag==0) return Math.Abs(c.real);
          }
          goto default;
        default: throw Ops.ArgError(name+": expected a real number, but received "+Ops.TypeName(obj));
      }
    }
  }
  #endregion

  #region acos
  public sealed class acos : Primitive
  { public acos() : base("acos", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      return obj is Complex ? Complex.Acos((Complex)obj) : (object)Math.Acos(Ops.ToFloat(obj));
    }
  }
  #endregion

  #region angle
  public sealed class angle : Primitive
  { public angle() : base("angle", 1, 1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      return Ops.ExpectComplex(args[0]).Angle;
    }
  }
  #endregion

  #region asin
  public sealed class asin : Primitive
  { public asin() : base("asin", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      return obj is Complex ? Complex.Asin((Complex)obj) : (object)Math.Asin(Ops.ToFloat(obj));
    }
  }
  #endregion

  #region atan
  public sealed class atan : Primitive
  { public atan() : base("atan", 1, 2) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args.Length==2) return Math.Atan2(Ops.ToFloat(args[0]), Ops.ToFloat(args[1]));

      object obj = args[0];
      return obj is Complex ? Complex.Atan((Complex)obj) : (object)Math.Atan(Ops.ToFloat(obj));
    }
  }
  #endregion

  #region ceiling
  public sealed class ceiling : Primitive
  { public ceiling() : base("ceiling", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      switch(Convert.GetTypeCode(obj))
      { case TypeCode.Byte:  case TypeCode.UInt16: case TypeCode.UInt32: case TypeCode.UInt64: 
        case TypeCode.SByte: case TypeCode.Int16:  case TypeCode.Int32:  case TypeCode.Int64:
          return obj;
        case TypeCode.Decimal:
        { Decimal d=(Decimal)obj, t=Decimal.Truncate(d);
          return d==t ? obj : t+Decimal.One;
        }
        case TypeCode.Double: return Math.Ceiling((double)obj);
        case TypeCode.Single: return Math.Ceiling((float)obj);
        case TypeCode.Object:
          if(obj is Integer) return obj;
          if(obj is Complex)
          { Complex c = (Complex)obj;
            if(c.imag==0) return Math.Ceiling(c.real);
          }
          goto default;
        default: throw Ops.ArgError(name+": expected a real number, but received "+Ops.TypeName(obj));
      }
    }
  }
  #endregion

  #region conjugate
  public sealed class conjugate : Primitive
  { public conjugate() : base("conjugate", 1, 1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      return Ops.ExpectComplex(args[0]).Conjugate;
    }
  }
  #endregion

  #region cos
  public sealed class cos : Primitive
  { public cos() : base("cos", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      return Math.Cos(Ops.ToFloat(args[0]));
    }
  }
  #endregion

  #region log
  public sealed class log : Primitive
  { public log() : base("log", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      return obj is Complex ? Complex.Log((Complex)obj) : (object)Math.Log(Ops.ToFloat(obj));
    }
  }
  #endregion

  #region log10
  public sealed class log10 : Primitive
  { public log10() : base("log10", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      return obj is Complex ? Complex.Log10((Complex)obj) : (object)Math.Log10(Ops.ToFloat(obj));
    }
  }
  #endregion

  #region magnitude
  public sealed class magnitude : Primitive
  { public magnitude() : base("magnitude", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      switch(Convert.GetTypeCode(obj))
      { case TypeCode.Byte: case TypeCode.UInt16: case TypeCode.UInt32: case TypeCode.UInt64: return obj;
        case TypeCode.Decimal: return Math.Abs((Decimal)obj);
        case TypeCode.Double: return Math.Abs((double)obj);
        case TypeCode.Int16: return Math.Abs((short)obj);
        case TypeCode.Int32: return Math.Abs((int)obj);
        case TypeCode.Int64: return Math.Abs((long)obj);
        case TypeCode.SByte: return Math.Abs((sbyte)obj);
        case TypeCode.Single: return Math.Abs((float)obj);
        case TypeCode.Object:
          if(obj is Integer) return ((Integer)obj).Abs;
          if(obj is Complex) return ((Complex)obj).Magnitude;
          goto default;
        default: throw Ops.ArgError(name+": expected a number, but received "+Ops.TypeName(obj));
      }
    }
  }
  #endregion

  #region make-polar
  public sealed class makePolar : Primitive
  { public makePolar() : base("make-polar", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      double phase = Ops.ToFloat(args[1]);
      return new Complex(Math.Cos(phase), Math.Sin(phase)) * Ops.ToFloat(args[0]);
    }
  }
  #endregion

  #region make-rectangular
  public sealed class makeRectangular : Primitive
  { public makeRectangular() : base("make-rectangular", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      return new Complex(Ops.ToFloat(args[0]), Ops.ToFloat(args[1]));
    }
  }
  #endregion

  #region exp
  public sealed class exp : Primitive
  { public exp() : base("exp", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      return Math.Exp(Ops.ToFloat(args[0]));
    }
  }
  #endregion

  #region floor
  public sealed class floor : Primitive
  { public floor() : base("floor", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      switch(Convert.GetTypeCode(obj))
      { case TypeCode.Byte:  case TypeCode.UInt16: case TypeCode.UInt32: case TypeCode.UInt64: 
        case TypeCode.SByte: case TypeCode.Int16:  case TypeCode.Int32:  case TypeCode.Int64:
          return obj;
        case TypeCode.Decimal: return Decimal.Floor((Decimal)obj);
        case TypeCode.Double: return Math.Floor((double)obj);
        case TypeCode.Single: return Math.Floor((float)obj);
        case TypeCode.Object:
          if(obj is Integer) return obj;
          if(obj is Complex)
          { Complex c = (Complex)obj;
            if(c.imag==0) return Math.Floor(c.real);
          }
          goto default;
        default: throw Ops.ArgError(name+": expected a real number, but received "+Ops.TypeName(obj));
      }
    }
  }
  #endregion

  #region round
  public sealed class round : Primitive
  { public round() : base("round", 1, 2) { }

    public override object Call(object[] args)
    { CheckArity(args);

      object obj = args[0];
      int places = args.Length==2 ? Ops.ToInt(args[1]) : 0;
      switch(Convert.GetTypeCode(obj))
      { case TypeCode.Byte:  case TypeCode.UInt16: case TypeCode.UInt32: case TypeCode.UInt64: 
        case TypeCode.SByte: case TypeCode.Int16:  case TypeCode.Int32:  case TypeCode.Int64:
          return obj;
        case TypeCode.Decimal: return Decimal.Round((Decimal)obj, places);
        case TypeCode.Double:  return Math.Round((double)obj, places);
        case TypeCode.Single:  return Math.Round((float)obj, places);
        case TypeCode.Object:
          if(obj is Integer) return obj;
          if(obj is Complex)
          { Complex c = (Complex)obj;
            if(c.imag==0) return Math.Round(c.real, places);
          }
          goto default;
        default: throw Ops.ArgError(name+": expected a real number, but received "+Ops.TypeName(obj));
      }
    }

    static object doubleCore(double d)
    { try { return checked((int)d); }
      catch(OverflowException)
      { try { return checked((long)d); }
        catch(OverflowException) { return new Integer(d); }
      }
    }
  }
  #endregion

  #region sin
  public sealed class sin : Primitive
  { public sin() : base("sin", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      return Math.Sin(Ops.ToFloat(args[0]));
    }
  }
  #endregion

  #region sqrt
  public sealed class sqrt : Primitive
  { public sqrt() : base("sqrt", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      return obj is Complex ? Complex.Sqrt((Complex)obj) : (object)Math.Sqrt(Ops.ToFloat(obj));
    }
  }
  #endregion

  #region tan
  public sealed class tan : Primitive
  { public tan() : base("tan", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      return Math.Tan(Ops.ToFloat(args[0]));
    }
  }
  #endregion

  #region truncate
  public sealed class truncate : Primitive
  { public truncate() : base("truncate", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      object obj = args[0];
      switch(Convert.GetTypeCode(obj))
      { case TypeCode.Byte:  case TypeCode.UInt16: case TypeCode.UInt32: case TypeCode.UInt64: 
        case TypeCode.SByte: case TypeCode.Int16:  case TypeCode.Int32:  case TypeCode.Int64:
          return obj;
        case TypeCode.Decimal: return Decimal.Truncate((Decimal)obj);
        case TypeCode.Double:  return doubleCore((double)obj);
        case TypeCode.Single:  return doubleCore((float)obj);
        case TypeCode.Object:
          if(obj is Integer) return obj;
          if(obj is Complex)
          { Complex c = (Complex)obj;
            if(c.imag==0) return doubleCore(c.real);
          }
          goto default;
        default: throw Ops.ArgError(name+": expected a real number, but received "+Ops.TypeName(obj));
      }
    }

    static object doubleCore(double d)
    { try { return checked((int)d); }
      catch(OverflowException)
      { try { return checked((long)d); }
        catch(OverflowException) { return new Integer(d); }
      }
    }
  }
  #endregion
}

} // namespace NetLisp.Mods