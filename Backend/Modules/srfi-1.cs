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
using Scripting;
using NetLisp.Backend;

namespace NetLisp.Mods
{

// TODO: optimize filter, member, etc. to share the longest common tails
// TODO: optimize methods that cons new lists so they don't do the if(head==null) ... thing inside the loop
// TODO: list sets
// FIXME: the srfi-1 module should also export srfi-1 names that are defined in Builtins

[LispCode(@"
(define first  car)
(define second cadr)
(define third  caddr)
(define fourth cadddr)
(define (fifth   x) (car    (cddddr x)))
(define (sixth   x) (cadr   (cddddr x)))
(define (seventh x) (caddr  (cddddr x)))
(define (eighth  x) (cadddr (cddddr x)))
(define (ninth   x) (car  (cddddr (cddddr x))))
(define (tenth   x) (cadr (cddddr (cddddr x))))

(define (alist-cons key datum alist) (cons (cons key datum) alist))

(define (append-map  f . lists) (apply append  (apply map f lists)))
(define (append-map! f . lists) (apply append! (apply map f lists)))

(define (append-reverse rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
        (lp (cdr rev-head) (cons (car rev-head) tail)))))

(define (append-reverse! rev-head tail)
  (let lp ((rev-head rev-head) (tail tail))
    (if (null-list? rev-head) tail
        (let ((next-rev (cdr rev-head)))
          (set-cdr! rev-head tail)
          (lp next-rev rev-head)))))

(define (circular-list val1 . vals)
  (let ((ans (cons val1 vals)))
    (set-cdr! (last-pair ans) ans)
    ans))

(define (concatenate lists) (apply append lists))
(define (concatenate! lists) (apply append! lists))

(define (delete-duplicates lis . maybe-=)
  (let ((elt= (if (null? maybe-=) equal? (car maybe-=))))
    (let recur ((lis lis))
      (if (null-list? lis) lis
          (let* ((x (car lis))
                 (tail (cdr lis))
                 (new-tail (recur (delete x tail elt=))))
            (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (delete-duplicates! lis . maybe-=)
  (let ((elt= (if (null? maybe-=) equal? (car maybe-=))))
    (let recur ((lis lis))
      (if (null-list? lis) lis
          (let* ((x (car lis))
                 (tail (cdr lis))
                 (new-tail (recur (delete! x tail elt=))))
            (if (eq? tail new-tail) lis (cons x new-tail)))))))

(define (drop-right lis k)
  (let recur ((lag lis) (lead (drop lis k)))
    (if (pair? lead)
        (cons (car lag) (recur (cdr lag) (cdr lead)))
        nil)))

(define (drop-right! lis k)
  (let ((lead (drop lis k)))
    (if (pair? lead)
        (let lp ((lag lis)  (lead (cdr lead)))
          (if (pair? lead)
              (lp (cdr lag) (cdr lead))
              (begin (set-cdr! lag '())
                     lis)))
        '())))

(define map-in-order map)

(define (not-pair? obj) (not (pair? obj)))

(define (null-list? l)
  (cond ((pair? l) #f)
        ((null? l) #t)
        (else (error ""null-list?: argument out of domain"" l))))

(define (proper-list? obj) (or (null? obj) (list? obj)))

(define (reduce f ridentity lis)
  (if (null-list? lis) ridentity
      (fold f (car lis) (cdr lis))))

(define (reduce-right f ridentity lis)
  (if (null-list? lis) ridentity
      (let recur ((head (car lis)) (lis (cdr lis)))
        (if (pair? lis)
            (f head (recur (car lis) (cdr lis)))
            head))))

(define (split-at x k) (values (take x k) (drop x k)))

(define (split-at! x k)
  (if (zero? k) (values nil x)
      (let* ((prev (drop x (- k 1)))
             (suffix (cdr prev)))
        (set-cdr! prev nil)
        (values x suffix))))

(define (take-right lis k)
  (let lp ((lag lis)  (lead (drop lis k)))
    (if (pair? lead)
        (lp (cdr lag) (cdr lead))
        lag)))

(define (unzip1 lis) (map car lis))

(define (unzip2 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis)
        (let ((elt (car lis)))
          (let-values (((a b) (recur (cdr lis))))
            (values (cons (car  elt) a)
                    (cons (cadr elt) b)))))))

(define (unzip3 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis)
        (let ((elt (car lis)))
          (let-values (((a b c) (recur (cdr lis))))
            (values (cons (car   elt) a)
                    (cons (cadr  elt) b)
                    (cons (caddr elt) c)))))))

(define (unzip4 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis)
        (let ((elt (car lis)))
          (let-values (((a b c d) (recur (cdr lis))))
            (values (cons (car    elt) a)
                    (cons (cadr   elt) b)
                    (cons (caddr  elt) c)
                    (cons (cadddr elt) d)))))))

(define (unzip5 lis)
  (let recur ((lis lis))
    (if (null-list? lis) (values lis lis lis lis lis)
        (let ((elt (car lis)))
          (let-values (((a b c d e) (recur (cdr lis))))
            (values (cons (car     elt) a)
                    (cons (cadr    elt) b)
                    (cons (caddr   elt) c)
                    (cons (cadddr  elt) d)
                    (cons (car (cddddr  elt)) e)))))))

(define (xcons a b) (cons b a))

(define (zip list1 . more-lists) (apply map list list1 more-lists))
")]
[ScriptName("srfi-1")]
public sealed class Srfi1
{ 
  #region alist-copy
  public sealed class alistCopy : Primitive
  { public alistCopy() : base("alist-copy", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args[0]==null) return null;
      Pair list=LispOps.ExpectPair(args[0]), head=null, tail=null;
      if(list==null) return null;
      do
      { Pair pair = list.Car as Pair;
        if(pair==null) throw Ops.ValueError(Name+": alists must contain only pairs");
        pair = new Pair(new Pair(pair.Car, pair.Cdr), null);
        if(head==null) head=tail=pair;
        else { tail.Cdr=pair; tail=pair; }
        list = list.Cdr as Pair;
      } while(list!=null);
      return head;
    }
  }
  #endregion

  #region alist-delete
  public sealed class alistDelete : Primitive
  { public alistDelete() : base("alist-delete", 2, 3) { }
    public override object Call(object[] args)
    { CheckArity(args);
      object obj=args[0];
      Pair  list=LispOps.ExpectList(args[1]), head=null, tail=null;
      if(args.Length==2)
        while(list!=null)
        { Pair pair = list.Car as Pair;
          if(pair==null) throw Ops.ValueError(Name+": alists must contain only pairs");
          if(!LispOps.EqualP(obj, pair.Car))
          { Pair next=new Pair(list.Car, null);
            if(head==null) head=tail=next;
            else { tail.Cdr=next; tail=next; }
          }
          list = list.Cdr as Pair;
        }
      else
      { IProcedure pred = Ops.ExpectProcedure(args[2]);
        bool realloc = pred.NeedsFreshArgs;
        if(!realloc)
        { args = new object[2];
          args[1] = obj;
        }
        while(list!=null)
        { Pair pair = list.Car as Pair;
          if(pair==null) throw Ops.ValueError(Name+": alists must contain only pairs");
          if(realloc) args = new object[2] { pair.Car, obj };
          else args[0] = pair.Car;
          if(!Ops.IsTrue(pred.Call(args)))
          { Pair next=new Pair(list.Car, null);
            if(head==null) head=tail=next;
            else { tail.Cdr=next; tail=next; }
          }
          list = list.Cdr as Pair;
        }
      }
      return head;
    }
  }
  #endregion
  #region alist-delete!
  public sealed class alistDeleteN : Primitive
  { public alistDeleteN() : base("alist-delete!", 2, 3) { }
    public override object Call(object[] args)
    { CheckArity(args);
      object obj=args[0];
      Pair  list=LispOps.ExpectList(args[1]), head=list, prev=null;
      if(args.Length==2)
        while(list!=null)
        { Pair pair = list.Car as Pair;
          if(pair==null) throw Ops.ValueError(Name+": alists must contain only pairs");
          Pair next = list.Cdr as Pair;
          if(LispOps.EqualP(obj, pair.Car))
          { if(prev==null) head=next;
            else prev.Cdr=next;
          }
          else prev=list;
          list = list.Cdr as Pair;
        }
      else
      { IProcedure pred = Ops.ExpectProcedure(args[2]);
        bool realloc = pred.NeedsFreshArgs;
        if(!realloc)
        { args = new object[2];
          args[1] = obj;
        }
        while(list!=null)
        { Pair pair = list.Car as Pair;
          if(pair==null) throw Ops.ValueError(Name+": alists must contain only pairs");
          if(realloc) args = new object[2] { pair.Car, obj };
          else args[0] = pair.Car;
          Pair next = list.Cdr as Pair;
          if(Ops.IsTrue(pred.Call(args)))
          { if(prev==null) head=next;
            else prev.Cdr=next;
          }
          else prev=list;
          list = list.Cdr as Pair;
        }
      }
      return head;
    }
  }
  #endregion

  #region any
  public sealed class any : Primitive
  { public any() : base("any", 1, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args.Length==1) return Ops.TRUE;

      IProcedure func = Ops.ExpectProcedure(args[0]);
      bool realloc =  func.NeedsFreshArgs;

      object ret;
      if(args.Length==2)
      { Pair p = LispOps.ExpectList(args[1]);
        if(!realloc) args = new object[1];
        while(p!=null)
        { if(realloc) args = new object[1];
          args[0] = p.Car;
          p = p.Cdr as Pair;
          if(Ops.IsTrue(ret=func.Call(args))) return ret;
        }
      }
      else
      { Pair[] pairs = new Pair[args.Length-1];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+1]);

        if(!realloc) args = new object[pairs.Length];

        while(true)
        { if(realloc) args = new object[pairs.Length];
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return null;
            args[i] = pairs[i].Car;
            pairs[i] = pairs[i].Cdr as Pair;
          }
          if(Ops.IsTrue(ret=func.Call(args))) return ret;
        }
      }

      return Ops.FALSE;
    }
  }
  #endregion

  #region break
  public sealed class @break : Primitive
  { public @break() : base("break", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred=Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=null, tail=null;
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(Ops.IsTrue(pred.Call(args))) return new MultipleValues(head, list);
        Pair next = new Pair(list.Car, null);
        if(head==null) head=tail=next;
        else { tail.Cdr=next; tail=next; }
        list = list.Cdr as Pair;
      }
      return new MultipleValues(head, null);
    }
  }
  #endregion

  #region break!
  public sealed class breakN : Primitive
  { public breakN() : base("break!", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred=Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=list, prev=null;
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(Ops.IsTrue(pred.Call(args)))
        { prev.Cdr = null;
          return new MultipleValues(head, list);
        }
        prev = list;
        list = list.Cdr as Pair;
      }
      return new MultipleValues(head, null);
    }
  }
  #endregion

  #region car+cdr
  public sealed class carAndCdr : Primitive
  { public carAndCdr() : base("car+cdr", 1, 1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      Pair pair = LispOps.ExpectPair(args[0]);
      return new MultipleValues(pair.Car, pair.Cdr);
    }
  }
  #endregion

  #region circular-list?
  public sealed class circularListP : Primitive
  { public circularListP() : base("circular-list?", 1, 1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      Pair pair = args[0] as Pair;
      return pair==null ? Ops.FALSE : Ops.FromBool(core(pair));
    }

    internal static bool core(Pair slow)
    { if(slow==null) return false;

      Pair fast = slow.Cdr as Pair;
      if(fast==null) return false;

      while(true)
      { if(slow==fast) return true;
        slow = (Pair)slow.Cdr;
        fast = fast.Cdr as Pair;
        if(fast==null) return false;
        fast = fast.Cdr as Pair;
        if(fast==null) return false;
      }
    }
  }
  #endregion

  #region cons*
  public sealed class consAll : Primitive
  { public consAll() : base("cons*", 1, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      int i = args.Length-1;
      object obj = args[i];
      while(i-- != 0) obj = new Pair(args[i], obj);
      return obj;
    }
  }
  #endregion

  #region count
  public sealed class count : Primitive
  { public count() : base("count", 2, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);

      IProcedure func = Ops.ExpectProcedure(args[0]);
      int count=0;
      bool realloc = func.NeedsFreshArgs;

      if(args.Length==2)
      { Pair p = LispOps.ExpectList(args[1]);
        if(!realloc) args = new object[1];
        while(p!=null)
        { if(realloc) args = new object[1];
          args[0] = p.Car;
          p = p.Cdr as Pair;
          if(Ops.IsTrue(func.Call(args))) count++;
        }
        return count;
      }
      else
      { Pair[] pairs = new Pair[args.Length-1];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+1]);

        if(!realloc) args = new object[pairs.Length];

        while(true)
        { if(realloc) args = new object[pairs.Length];
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return count;
            args[i] = pairs[i].Car;
            pairs[i] = pairs[i].Cdr as Pair;
          }
          if(Ops.IsTrue(func.Call(args))) count++;
        }
      }
    }
  }
  #endregion

  #region delete
  public sealed class delete : Primitive
  { public delete() : base("delete", 2, 3) { }
    public override object Call(object[] args)
    { CheckArity(args);
      object obj=args[0];
      Pair  list=LispOps.ExpectList(args[1]), head=null, tail=null;
      if(args.Length==2)
        while(list!=null)
        { if(!LispOps.EqualP(obj, list.Car))
          { Pair next=new Pair(list.Car, null);
            if(head==null) head=tail=next;
            else { tail.Cdr=next; tail=next; }
          }
          list = list.Cdr as Pair;
        }
      else
      { IProcedure pred = Ops.ExpectProcedure(args[2]);
        bool realloc = pred.NeedsFreshArgs;
        if(!realloc)
        { args = new object[2];
          args[1] = obj;
        }
        while(list!=null)
        { if(realloc) args = new object[2] { list.Car, obj };
          else args[0] = list.Car;
          if(!Ops.IsTrue(pred.Call(args)))
          { Pair next=new Pair(list.Car, null);
            if(head==null) head=tail=next;
            else { tail.Cdr=next; tail=next; }
          }
          list = list.Cdr as Pair;
        }
      }
      return head;
    }
  }
  #endregion
  #region delete!
  public sealed class deleteN : Primitive
  { public deleteN() : base("delete!", 2, 3) { }
    public override object Call(object[] args)
    { CheckArity(args);
      object obj=args[0];
      Pair  list=LispOps.ExpectList(args[1]), head=list, prev=null;
      if(list==null) return null;

      if(args.Length==2)
        do
        { object cobj = list.Car;
          Pair next = list.Cdr as Pair;
          if(LispOps.EqualP(obj, cobj))
          { if(prev==null) head=next;
            else prev.Cdr=next;
          }
          else prev=list;
          list = next;
        } while(list!=null);
      else
      { IProcedure pred = Ops.ExpectProcedure(args[2]);
        bool realloc = pred.NeedsFreshArgs;
        if(!realloc)
        { args = new object[2];
          args[1] = obj;
        }
        do
        { if(realloc) args = new object[2] { list.Car, obj };
          else args[0] = list.Car;
          Pair next = list.Cdr as Pair;
          if(Ops.IsTrue(pred.Call(args)))
          { if(prev==null) head=next;
            else prev.Cdr=next;
          }
          else prev=list;
          list = next;
        } while(list!=null);
      }
      return head;
    }
  }
  #endregion

  #region dotted-list?
  public sealed class dottedListP : Primitive
  { public dottedListP() : base("dotted-list?", 1, 1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      Pair slow = args[0] as Pair;
      if(slow==null) return Ops.FALSE;

      Pair fast = slow.Cdr as Pair;
      if(fast==null) return slow.Cdr==null ? Ops.FALSE : Ops.TRUE;

      while(true)
      { if(slow==fast) return Ops.FALSE;
        slow = (Pair)slow.Cdr;
        Pair next = fast.Cdr as Pair;
        if(next==null) return fast.Cdr==null ? Ops.FALSE : Ops.TRUE;
        fast = next.Cdr as Pair;
        if(fast==null) return next.Cdr==null ? Ops.FALSE : Ops.TRUE;
      }
    }
  }
  #endregion

  #region drop
  public sealed class drop : Primitive
  { public drop() : base("drop", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      return core(Name, LispOps.ExpectList(args[0]), Ops.ExpectInt(args[1]));
    }

    internal static Pair core(string name, Pair pair, int length)
    { for(int i=0; i<length; i++)
      { Pair next = pair.Cdr as Pair;
        if(next==null)
        { if(pair.Cdr==null && i==length-1) return null;
          throw new ArgumentException(name+": list is not long enough");
        }
        pair = next;
      }
      return pair;
    }
  }
  #endregion

  #region drop-while
  public sealed class dropWhile : Primitive
  { public dropWhile() : base("drop-while", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred = Ops.ExpectProcedure(args[0]);
      Pair list = LispOps.ExpectList(args[1]);
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(!Ops.IsTrue(pred.Call(args))) return list;
        list = list.Cdr as Pair;
      }
      return null;
    }
  }
  #endregion

  #region every
  public sealed class every : Primitive
  { public every() : base("every", 1, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args.Length==1) return Ops.TRUE;

      IProcedure func = Ops.ExpectProcedure(args[0]);
      bool realloc = func.NeedsFreshArgs;

      object ret=Ops.TRUE;
      if(args.Length==2)
      { Pair p = LispOps.ExpectList(args[1]);
        if(!realloc) args = new object[1];
        while(p!=null)
        { if(realloc) args = new object[1];
          args[0] = p.Car;
          p = p.Cdr as Pair;
          ret=func.Call(args);
          if(ret is bool && !(bool)ret) return Ops.FALSE;
        }
      }
      else
      { Pair[] pairs = new Pair[args.Length-1];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+1]);

        if(!realloc) args = new object[pairs.Length];

        while(true)
        { if(realloc) args = new object[pairs.Length];
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return null;
            args[i] = pairs[i].Car;
            pairs[i] = pairs[i].Cdr as Pair;
          }
          ret=func.Call(args);
          if(ret is bool && !(bool)ret) return Ops.FALSE;
        }
      }

      return ret;
    }
  }
  #endregion

  #region filter
  public sealed class filter : Primitive
  { public filter() : base("filter", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure proc = Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=null, tail=null;
      if(list==null) return null;
      bool realloc = proc.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      do
      { if(!realloc) args = new object[1];
        args[0] = list.Car;
        if(Ops.IsTrue(proc.Call(args)))
        { Pair next = new Pair(list.Car, null);
          if(head==null) head=tail=next;
          else { tail.Cdr=next; tail=next; }
        }
        list = list.Cdr as Pair;
      } while(list!=null);
      return head;
    }
  }
  #endregion

  #region filter!
  public sealed class filterN : Primitive
  { public filterN() : base("filter!", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure proc = Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=list, prev=null;
      if(list==null) return null;
      bool realloc = proc.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      do
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        list = list.Cdr as Pair;
        if(!Ops.IsTrue(proc.Call(args)))
        { if(prev==null) head=list;
          else prev.Cdr=list;
        }
        else prev=list;
      } while(list!=null);
      return head;
    }
  }
  #endregion

  #region filter-map
  public sealed class filterMap : Primitive
  { public filterMap() : base("filter-map", 1, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args.Length==1) return null;

      IProcedure func = Ops.ExpectProcedure(args[0]);
      object value;
      bool realloc = func.NeedsFreshArgs;

      if(args.Length==2)
      { Pair p=LispOps.ExpectList(args[1]), head=null, tail=null;
        if(!realloc) args = new object[1];
        while(p!=null)
        { if(realloc) args = new object[1];
          args[0] = p.Car;
          p = p.Cdr as Pair;
          value = func.Call(args);
          if(!Ops.IsTrue(value)) continue;
          Pair next = new Pair(value, null);
          if(head==null) head=tail=next;
          else { tail.Cdr=next; tail=next; }
        }
        return head;
      }
      else
      { Pair[] pairs = new Pair[args.Length-1];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+1]);

        if(!realloc) args = new object[pairs.Length];

        Pair head=null, tail=null;
        while(true)
        { if(realloc) args = new object[pairs.Length];
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return head;
            args[i] = pairs[i].Car;
            pairs[i] = pairs[i].Cdr as Pair;
          }
          value = func.Call(args);
          if(!Ops.IsTrue(value)) continue;
          Pair next = new Pair(value, null);
          if(head==null) head=tail=next;
          else { tail.Cdr=next; tail=next; }
        }
      }
    }
  }
  #endregion

  #region find
  public sealed class find : Primitive
  { public find() : base("find", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred = Ops.ExpectProcedure(args[0]);
      Pair list = LispOps.ExpectList(args[1]);
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(Ops.IsTrue(pred.Call(args))) return list.Car;
        list = list.Cdr as Pair;
      }
      return Ops.FALSE;
    }
  }
  #endregion

  #region find-tail
  public sealed class findTail : Primitive
  { public findTail() : base("find-tail", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred = Ops.ExpectProcedure(args[0]);
      Pair list = LispOps.ExpectList(args[1]);
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(Ops.IsTrue(pred.Call(args))) return list;
        list = list.Cdr as Pair;
      }
      return Ops.FALSE;
    }
  }
  #endregion

  #region fold
  public sealed class fold : Primitive
  { public fold() : base("fold", 3, -1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure kons=Ops.ExpectProcedure(args[0]);
      object ans=args[1];
      bool realloc=kons.NeedsFreshArgs;

      if(args.Length==3)
      { Pair pair = LispOps.ExpectList(args[2]);
        if(realloc) args = new object[2];
        while(true)
        { if(pair==null) return ans;
          if(!realloc) args = new object[2];
          args[0] = pair.Car;
          args[1] = ans;
          ans = kons.Call(args);
          pair = pair.Cdr as Pair;
        }
      }
      else
      { Pair[] pairs = new Pair[args.Length-2];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+2]);

        args = new object[pairs.Length+1];
        if(!realloc) args[pairs.Length] = ans;
        while(true)
        { if(realloc) args[pairs.Length] = ans;
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return args[pairs.Length];
            args[i]  = pairs[i].Car;
            pairs[i] = pairs[i].Cdr as Pair;
          }
          args[pairs.Length] = kons.Call(args);
        }
      }
    }
  }
  #endregion

  #region fold-right
  public sealed class foldRight : Primitive
  { public foldRight() : base("fold-right", 3, -1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure kons = Ops.ExpectProcedure(args[0]);
      object ans = args[1];

      if(args.Length==3) return new folder1(kons, ans).Run(LispOps.ExpectList(args[2]));
      else
      { Pair[] pairs = new Pair[args.Length-2];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+2]);
        return new folderN(kons, ans, args.Length-2).Run(pairs);
      }
    }

    struct folder1
    { public folder1(IProcedure kons, object ans)
      { this.kons=kons; this.ans=ans; args=kons.NeedsFreshArgs ? null : new object[2];
      }

      public object Run(Pair pair)
      { if(pair==null) return ans;
        object[] args = this.args;
        if(args==null) args = new object[2];
        args[1] = Run(pair.Cdr as Pair);
        args[0] = pair.Car;
        return kons.Call(args);
      }

      object[] args;
      object ans;
      IProcedure kons;
    }

    struct folderN
    { public folderN(IProcedure kons, object ans, int nlists)
      { this.kons=kons; this.ans=ans;
        if(kons.NeedsFreshArgs) { args=null; nargs=nlists+1; }
        else { args=new object[nlists+1]; this.nargs=-1; }
      }

      public object Run(Pair[] pairs)
      { Pair[] next = new Pair[pairs.Length];
        object[] args = this.args;
        if(args==null) args = new object[nargs];
        for(int i=0; i<pairs.Length; i++)
          if((next[i]=pairs[i].Cdr as Pair)==null) { args[pairs.Length]=ans; goto skip; }
        args[pairs.Length] = Run(next);
        skip:
        for(int i=0; i<pairs.Length; i++) args[i] = pairs[i].Car;
        return kons.Call(args);
      }

      object[] args;
      object ans;
      IProcedure kons;
      int nargs;
    }
  }
  #endregion

  #region list-tabulate
  public sealed class listTabulate : Primitive
  { public listTabulate() : base("list-tabulate", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      int length = Ops.ExpectInt(args[0]);
      IProcedure proc = Ops.ExpectProcedure(args[1]);
      if(length<=0) return null;

      Pair head=null, tail=null;
      bool realloc = proc.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      for(int i=0; i<length; i++)
      { if(realloc) args = new object[1];
        args[0] = i;
        Pair next = new Pair(proc.Call(args), null);
        if(head==null) head=tail=next;
        else { tail.Cdr=next; tail=next; }
      }
      return head;
    }
  }
  #endregion

  #region iota
  public sealed class iota : Primitive
  { public iota() : base("iota", 1, 3) { }
    public override object Call(object[] args)
    { CheckArity(args);
      int count = Ops.ToInt(args[0]);
      if(count==0) return null;
      if(count<0) throw Ops.ValueError(Name+": count cannot be negative");
      object start=args.Length<2 ? 0 : args[1], step=args.Length<3 ? 1 : args[2];
      // this can be optimized with type-specific paths (eg, int and float)
      Pair head=new Pair(start, null), tail=head;
      while(--count!=0)
      { start = Ops.Add(start, step);
        Pair next = new Pair(start, null);
        tail.Cdr=next; tail=next;
      }
      return head;
    }
  }
  #endregion

  #region last
  public sealed class last : Primitive
  { public last() : base("last", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      return lastPair.core(LispOps.ExpectPair(args[0])).Car;
    }
  }
  #endregion

  #region last-pair
  public sealed class lastPair : Primitive
  { public lastPair() : base("last-pair", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      return args[0]==null ? null : core(LispOps.ExpectPair(args[0]));
    }

    internal static Pair core(Pair pair)
    { while(true)
      { Pair next = pair.Cdr as Pair;
        if(next==null) return pair;
        pair = next;
      }
    }
  }
  #endregion

  #region length+
  public sealed class lengthPlus : Primitive
  { public lengthPlus() : base("length+", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      Pair slow = LispOps.ExpectList(args[0]);
      if(slow==null) return 0;

      Pair fast = slow.Cdr as Pair;
      if(fast==null) return 1;

      int length=1;
      while(true)
      { if(slow==fast) return Ops.FALSE;
        slow = (Pair)slow.Cdr;
        fast = fast.Cdr as Pair;
        if(fast==null) return length+1;
        fast = fast.Cdr as Pair;
        length += 2;
        if(fast==null) return length;
      }
    }
  }
  #endregion

  #region list-copy
  public sealed class listCopy : Primitive
  { public listCopy() : base("list-copy", 1, 1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args[0]==null) return null;
      Pair list=LispOps.ExpectPair(args[0]), head=new Pair(list.Car, list.Cdr), tail=head;
      while(true)
      { list = list.Cdr as Pair;
        if(list==null) return head;
        Pair next = new Pair(list.Car, list.Cdr);
        tail.Cdr = next;
        tail = next;
      }
    }
  }
  #endregion

  #region list-index
  public sealed class listIndex : Primitive
  { public listIndex() : base("list-index", 2, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure func = Ops.ExpectProcedure(args[0]);
      bool realloc = func.NeedsFreshArgs;

      int index=0;
      if(args.Length==2)
      { Pair p = LispOps.ExpectList(args[1]);
        if(!realloc) args = new object[1];
        while(p!=null)
        { if(realloc) args = new object[1];
          args[0] = p.Car;
          p = p.Cdr as Pair;
          if(Ops.IsTrue(func.Call(args))) return index;
          index++;
        }
        return Ops.FALSE;
      }
      else
      { Pair[] pairs = new Pair[args.Length-1];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+1]);

        if(!realloc) args = new object[pairs.Length];

        while(true)
        { if(realloc) args = new object[pairs.Length];
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return Ops.FALSE;
            args[i] = pairs[i].Car;
            pairs[i] = pairs[i].Cdr as Pair;
          }
          if(Ops.IsTrue(func.Call(args))) return index;
          index++;
        }
      }
    }
  }
  #endregion

  #region list=
  public sealed class listEq : Primitive
  { public listEq() : base("list=", 1, -1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      if(args.Length<3) return Ops.TRUE;

      IProcedure test = Ops.ExpectProcedure(args[0]);
      Pair last=LispOps.ExpectList(args[1]);
      bool realloc = test.NeedsFreshArgs;

      object[] nargs = realloc ? null : new object[2];

      for(int i=2; i<args.Length; i++)
      { Pair cur=LispOps.ExpectList(args[i]), cpair=cur;
        while(true)
        { if(cpair==null)
          { if(last!=null) return Ops.FALSE;
            else break;
          }
          else if(last==null) return Ops.FALSE;

          if(realloc) nargs = new object[2];
          nargs[0]=last.Car; nargs[1]=cpair.Car;
          if(!Ops.IsTrue(test.Call(nargs))) return Ops.FALSE;

          last=last.Cdr as Pair; cpair=cpair.Cdr as Pair;
        }
        last=cur;
      }

      return Ops.TRUE;
    }
  }
  #endregion

  #region make-list
  public sealed class makeList : Primitive
  { public makeList() : base("make-list", 1, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      int length  = Ops.ExpectInt(args[0]);
      object fill = args.Length==2 ? args[1] : null;

      Pair head=null, tail=null;
      for(int i=0; i<length; i++)
      { Pair next = new Pair(fill, null);
        if(head==null) head=tail=next;
        else { tail.Cdr=next; tail=next; }
      }
      return head;
    }
  }
  #endregion

  #region map!
  public sealed class mapN : Primitive
  { public mapN() : base("map!", 1, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args.Length==1) return null;

      IProcedure func = Ops.ExpectProcedure(args[0]);
      bool realloc = func.NeedsFreshArgs;

      if(args.Length==2)
      { Pair head=LispOps.ExpectList(args[1]), tail=head;
        if(!realloc) args = new object[1];
        while(tail!=null)
        { if(realloc) args = new object[1];
          args[0]  = tail.Car;
          tail.Car = func.Call(args);
          tail = tail.Cdr as Pair;
        }
        return head;
      }
      else
      { Pair[] pairs = new Pair[args.Length-1];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+1]);

        if(!realloc) args = new object[pairs.Length];

        Pair head=pairs[0], tail=head;
        while(tail!=null)
        { if(realloc) args = new object[pairs.Length];
          args[0] = tail.Car;
          for(int i=1; i<pairs.Length; i++)
          { if(pairs[i]==null) goto done;
            args[i] = pairs[i].Car;
            pairs[i] = pairs[i].Cdr as Pair;
          }
          tail.Car = func.Call(args);
          tail = tail.Cdr as Pair;
        }
        done: return head;
      }
    }
  }
  #endregion

  #region pair-fold
  public sealed class pairFold : Primitive
  { public pairFold() : base("pair-fold", 3, -1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure kons=Ops.ExpectProcedure(args[0]);
      object ans=args[1];
      bool realloc = kons.NeedsFreshArgs;

      if(args.Length==3)
      { Pair pair = LispOps.ExpectList(args[2]);
        if(!realloc) args = new object[2];
        while(true)
        { if(pair==null) return ans;
          if(realloc) args = new object[2];
          args[0] = pair;
          args[1] = ans;
          Pair next = pair.Cdr as Pair;
          ans  = kons.Call(args);
          pair = next;
        }
      }
      else
      { Pair[] pairs = new Pair[args.Length-2];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+2]);

        if(!realloc)
        { args = new object[pairs.Length+1];
          args[pairs.Length] = ans;
        }
        while(true)
        { if(realloc)
          { args = new object[pairs.Length+1];
            args[pairs.Length] = ans;
          }
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return args[pairs.Length];
            args[i]  = pairs[i];
            pairs[i] = pairs[i].Cdr as Pair;
          }
          args[pairs.Length] = kons.Call(args);
        }
      }
    }
  }
  #endregion

  #region pair-fold-right
  public sealed class pairFoldRight : Primitive
  { public pairFoldRight() : base("pair-fold-right", 3, -1) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure kons=Ops.ExpectProcedure(args[0]);
      object ans=args[1];

      if(args.Length==3) return new folder1(kons, ans).Run(LispOps.ExpectList(args[2]));
      else
      { Pair[] pairs = new Pair[args.Length-2];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+2]);
        return new folderN(kons, ans, args.Length-2).Run(pairs);
      }
    }

    struct folder1
    { public folder1(IProcedure kons, object ans)
      { this.kons=kons; this.ans=ans; args=kons.NeedsFreshArgs ? null : new object[2];
      }

      public object Run(Pair pair)
      { if(pair==null) return ans;
        object[] args = this.args;
        if(args==null) args = new object[2];
        args[1] = Run(pair.Cdr as Pair);
        args[0] = pair;
        return kons.Call(args);
      }

      object[] args;
      object ans;
      IProcedure kons;
    }

    struct folderN
    { public folderN(IProcedure kons, object ans, int nlists)
      { this.kons=kons; this.ans=ans;
        if(kons.NeedsFreshArgs) { args=null; nargs=nlists+1; }
        else { args=new object[nlists+1]; nargs=-1; }
      }

      public object Run(Pair[] pairs)
      { Pair[] next = new Pair[pairs.Length];
        object[] args = this.args;
        if(args==null) args = new object[nargs];
        for(int i=0; i<pairs.Length; i++)
          if((next[i]=pairs[i].Cdr as Pair)==null) { args[pairs.Length]=ans; goto skip; }
        args[pairs.Length] = Run(next);
        skip: for(int i=0; i<pairs.Length; i++) args[i] = pairs[i];
        return kons.Call(args);
      }

      object[] args;
      object ans;
      IProcedure kons;
      int nargs;
    }
  }
  #endregion

  #region pair-for-each
  public sealed class pairForEach : Primitive
  { public pairForEach() : base("pair-for-each", 1, -1) { }

    public override object Call(object[] args)
    { CheckArity(args);
      if(args.Length==1) return null;

      IProcedure func = Ops.ExpectProcedure(args[0]);
      bool realloc = func.NeedsFreshArgs;
      if(args.Length==2)
      { Pair p = LispOps.ExpectList(args[1]);
        if(!realloc) args = new object[1];
        while(p!=null)
        { if(realloc) args = new object[1];
          args[0] = p;
          p = p.Cdr as Pair;
          func.Call(args);
        }
        return null;
      }
      else
      { Pair[] pairs = new Pair[args.Length-1];
        for(int i=0; i<pairs.Length; i++) pairs[i] = LispOps.ExpectList(args[i+1]);

        if(!realloc) args = new object[pairs.Length];
        while(true)
        { if(realloc) args = new object[pairs.Length];
          for(int i=0; i<pairs.Length; i++)
          { if(pairs[i]==null) return null;
            args[i] = pairs[i];
            pairs[i] = pairs[i].Cdr as Pair;
          }
          func.Call(args);
        }
      }
    }
  }
  #endregion

  #region partition
  public sealed class partition : Primitive
  { public partition() : base("partition", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure proc = Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), inlist=null, outlist=null, intail=null, outtail=null;
      bool realloc = proc.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { Pair next = new Pair(list.Car, null);
        if(realloc) args = new object[1];
        args[0] = list.Car;
        if(Ops.IsTrue(proc.Call(args)))
        { if(inlist==null) inlist=intail=next;
          else { intail.Cdr=next; intail=next; }
        }
        else if(outlist==null) outlist=outtail=next;
        else { outtail.Cdr=next; outtail=next; }
        list = list.Cdr as Pair;
      }
      return new MultipleValues(inlist, outlist);
    }
  }
  #endregion

  #region partition!
  public sealed class partitionN : Primitive
  { public partitionN() : base("partition!", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure proc = Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), inlist=null, outlist=null, inprev=null, outprev=null;
      bool realloc = proc.NeedsFreshArgs;
      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(Ops.IsTrue(proc.Call(args)))
        { if(inprev==null) inlist=inprev=list;
          else { inprev.Cdr=list; inprev=list; }
        }
        else if(outprev==null) outlist=outprev=list;
        else { outprev.Cdr=list; outprev=list; }
        list = list.Cdr as Pair;
      }
      if(inprev!=null) inprev.Cdr=null;
      if(outprev!=null) outprev.Cdr=null;
      return new MultipleValues(inlist, outlist);
    }
  }
  #endregion

  #region remove
  public sealed class remove : Primitive
  { public remove() : base("remove", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure proc = Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=null, tail=null;
      if(list==null) return null;
      bool realloc = proc.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      do
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(!Ops.IsTrue(proc.Call(args)))
        { Pair next = new Pair(list.Car, null);
          if(head==null) head=tail=next;
          else { tail.Cdr=next; tail=next; }
        }
        list = list.Cdr as Pair;
      } while(list!=null);
      return head;
    }
  }
  #endregion

  #region remove!
  public sealed class removeN : Primitive
  { public removeN() : base("remove!", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure proc = Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=list, prev=null;
      if(list==null) return null;
      bool realloc = proc.NeedsFreshArgs;
      if(!realloc) args = new object[1];
      do
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        Pair next = list.Cdr as Pair;
        if(Ops.IsTrue(proc.Call(args)))
        { if(prev==null) head=next;
          else prev.Cdr=next;
        }
        else prev=list;
        list = next;
      } while(list!=null);
      return head;
    }
  }
  #endregion

  #region span
  public sealed class span : Primitive
  { public span() : base("span", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred=Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=null, tail=null;
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(!Ops.IsTrue(pred.Call(args))) return new MultipleValues(head, list);
        Pair next = new Pair(list.Car, null);
        if(head==null) head=tail=next;
        else { tail.Cdr=next; tail=next; }
        list = list.Cdr as Pair;
      }
      return new MultipleValues(head, null);
    }
  }
  #endregion

  #region span!
  public sealed class spanN : Primitive
  { public spanN() : base("span!", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred=Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=list, prev=null;
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(!Ops.IsTrue(pred.Call(args)))
        { prev.Cdr = null;
          return new MultipleValues(head, list);
        }
        prev = list;
        list = list.Cdr as Pair;
      }
      return new MultipleValues(head, null);
    }
  }
  #endregion

  #region take
  public sealed class take : Primitive
  { public take() : base("take", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      return core(Name, LispOps.ExpectList(args[0]), Ops.ExpectInt(args[1]));
    }

    internal static Pair core(string name, Pair pair, int length)
    { if(length<=0) return null;

      Pair head=null, tail=null;
      do
      { if(pair==null) throw new ArgumentException(name+": list is not long enough");
        Pair next = new Pair(pair.Car, null);
        if(head==null) head=tail=next;
        else { tail.Cdr=next; tail=next; }
        pair = pair.Cdr as Pair;
      } while(--length != 0);
      return head;
    }
  }
  #endregion

  #region take!
  public sealed class takeN : Primitive
  { public takeN() : base("take!", 2, 2) { }
    public override object Call(object[] args)
    { CheckArity(args);
      Pair  head = LispOps.ExpectList(args[0]), pair=head;
      int length = Ops.ExpectInt(args[1]);
      if(length<=0) return null;
      while(--length != 0)
      { if(pair==null) throw new ArgumentException(Name+": list is not long enough");
        pair = pair.Cdr as Pair;
      }
      pair.Cdr = null;
      return head;
    }
  }
  #endregion

  #region take-while
  public sealed class takeWhile : Primitive
  { public takeWhile() : base("take-while", 2, 2) { }

    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred=Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=null, tail=null;
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(!Ops.IsTrue(pred.Call(args))) break;
        Pair next = new Pair(list.Car, null);
        if(head==null) head=tail=next;
        else { tail.Cdr=next; tail=next; }
        list = list.Cdr as Pair;
      }
      return head;
    }
  }
  #endregion

  #region take-while!
  public sealed class takeWhileN : Primitive
  { public takeWhileN() : base("take-while!", 2, 2) { }

    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure pred=Ops.ExpectProcedure(args[0]);
      Pair list=LispOps.ExpectList(args[1]), head=list, prev=null;
      bool realloc = pred.NeedsFreshArgs;

      if(!realloc) args = new object[1];
      while(list!=null)
      { if(realloc) args = new object[1];
        args[0] = list.Car;
        if(!Ops.IsTrue(pred.Call(args)))
        { if(prev==null) return null;
          else { prev.Cdr=null; return head; }
        }
        prev = list;
        list = list.Cdr as Pair;
      }
      return head;
    }
  }
  #endregion

  #region unfold
  public sealed class unfold : Primitive
  { public unfold() : base("unfold", 4, 5) { }
    public override object Call(object[] args)
    { CheckArity(args);
      return new unfolder(args).Run(args[3]);
    }

    struct unfolder
    { public unfolder(object[] args)
      { stop=Ops.ExpectProcedure(args[0]);  val=Ops.ExpectProcedure(args[1]);
        next=Ops.ExpectProcedure(args[2]); tail=args.Length==4 ? null : Ops.ExpectProcedure(args[4]);
        seed=new object[1];
        realloc = stop.NeedsFreshArgs || val.NeedsFreshArgs || next.NeedsFreshArgs ||
                  tail!=null && tail.NeedsFreshArgs;
      }

      public object Run(object seedval)
      { seed[0] = seedval;
        if(!realloc)
        { if(Ops.IsTrue(stop.Call(seed))) return tail==null ? null : tail.Call(seed);
          return new Pair(val.Call(seed), Run(next.Call(seed)));
        }
        else
        { object[] nseed = this.seed;
          if(stop.NeedsFreshArgs) nseed = new object[1] { seedval };
          if(Ops.IsTrue(stop.Call(nseed)))
          { if(tail==null) return null;
            if(tail.NeedsFreshArgs) nseed = new object[1] { seedval };
            return tail.Call(nseed);
          }
          if(val.NeedsFreshArgs) nseed = new object[1] { seedval };
          object value = val.Call(nseed);
          if(next.NeedsFreshArgs) nseed = new object[1] { seedval };
          return new Pair(value, Run(next.Call(nseed)));
        }
      }

      IProcedure stop, val, next, tail;
      object[] seed;
      bool realloc;
    }
  }
  #endregion

  #region unfold-right
  public sealed class unfoldRight : Primitive
  { public unfoldRight() : base("unfold-right", 4, 5) { }
    public override object Call(object[] args)
    { CheckArity(args);
      IProcedure stop=Ops.ExpectProcedure(args[0]), val=Ops.ExpectProcedure(args[1]),
                 next=Ops.ExpectProcedure(args[2]);
      object list=args.Length==4 ? null : args[4];
      object[] seed = new object[1] { args[3] };
      bool realloc = stop.NeedsFreshArgs || val.NeedsFreshArgs || next.NeedsFreshArgs;

      if(!realloc)
        while(!Ops.IsTrue(stop.Call(seed)))
        { list = new Pair(val.Call(seed), list);
          seed[0] = next.Call(seed);
        }
      else
      { object seedval = args[3];
        while(true)
        { if(stop.NeedsFreshArgs) seed = new object[1] { seedval };
          if(Ops.IsTrue(stop.Call(seed))) break;
          if(val.NeedsFreshArgs) seed = new object[1] { seedval };
          list = new Pair(val.Call(seed), list);
          if(next.NeedsFreshArgs) seed = new object[1] { seedval };
          seed[0] = seedval = next.Call(seed);
        }
      }

      return list;
    }
  }
  #endregion
}

} // namespace NetLisp.Mods