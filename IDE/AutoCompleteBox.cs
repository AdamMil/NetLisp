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
using System.Drawing;
using System.Windows.Forms;
using NetLisp.Backend;

namespace NetLisp.IDE
{

enum AcType { Class, Event, Field, Method, Namespace, Property };

public class AutoCompleteItem
{ public AutoCompleteItem(object obj, string name)
  { this.name=name;

    if(obj is FieldWrapper) type=AcType.Field;
    else if(obj is IProcedure || obj is ReflectedFunctions) type=AcType.Method;
    else if(obj is Type || obj is ReflectedType) type=AcType.Class;
    else if(obj is ReflectedNamespace) type=AcType.Namespace;
    else type=AcType.Method;
  }

  public override string ToString() { return name; }

  internal string name;
  internal AcType type;
}

public class AutoCompleteBox : ListBox
{ public AutoCompleteBox() { DrawMode = DrawMode.OwnerDrawFixed; }

  static AutoCompleteBox()
  { System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(AutoCompleteBox));
    images = new ImageList();
    images.ColorDepth = ColorDepth.Depth8Bit;
    images.ImageSize = new Size(16, 16);
    images.ImageStream = ((System.Windows.Forms.ImageListStreamer)(resources.GetObject("imageList.ImageStream")));
  }

  public override int ItemHeight
  { get { return System.Math.Max(base.ItemHeight, images.ImageSize.Height); }
    set { base.ItemHeight = value; }
  }

  protected override void OnDoubleClick(System.EventArgs e)
  { if(SelectedIndex != -1)
    { LispBox textbox = (LispBox)Tag;
      textbox.SelectItem();
      Hide();
      textbox.Focus();
    }
    base.OnDoubleClick(e);
  }

  protected override void OnDrawItem(DrawItemEventArgs e)
  { e.DrawBackground();
    e.DrawFocusRectangle();

    AutoCompleteItem item = (AutoCompleteItem)Items[e.Index];
    e.Graphics.DrawImageUnscaled(images.Images[(int)item.type], e.Bounds.Location);
    using(Brush brush=new SolidBrush(e.ForeColor))
      e.Graphics.DrawString(item.name, e.Font, brush, e.Bounds.X+images.ImageSize.Width, e.Bounds.Y);
    base.OnDrawItem(e);
  }

  protected override void OnKeyDown(KeyEventArgs e)
  { ((Control)Tag).Focus();
    base.OnKeyDown(e);
  }

  protected override void OnSelectedIndexChanged(System.EventArgs e)
  { if(Tag!=null) ((Control)Tag).Focus();
    base.OnSelectedIndexChanged(e);
  }

  static ImageList images;
}

} // namespace NetLisp.IDE
