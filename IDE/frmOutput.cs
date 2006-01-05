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
using System.IO;
using System.Drawing;
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace NetLisp.IDE
{

public class OutputForm : System.Windows.Forms.Form
{ public OutputForm()
	{ InitializeComponent();
		Console.SetOut(new Writer(textBox));
	}

  sealed class Writer : TextWriter
  { public Writer(System.Windows.Forms.TextBox textBox) { box=textBox; }

    public override System.Text.Encoding Encoding
    { get { return System.Text.Encoding.Unicode; }
    }

    public override void Write(char value)
    { if(App.MainForm.RedirectStdout)
      { if(value=='\r') return; // avoid a bug in ICSharpCode.TextEditor that causes two newlines to be added
        EditForm form = App.MainForm.ActiveMdiChild as EditForm;
        if(form!=null)
        { ICSharpCode.TextEditor.Document.IDocument doc = form.immediate.Document;
          doc.Insert(doc.TextLength, value.ToString());
          return;
        }
      }

      bool end = box.SelectionStart==box.TextLength;
      box.AppendText(value.ToString());
      if(end)
      { box.SelectionStart = box.TextLength;
        box.SelectionLength = 0;
      }
    }
    
    public override void Write(string value)
    { if(App.MainForm.RedirectStdout)
      { EditForm form = App.MainForm.ActiveMdiChild as EditForm;
        if(form!=null)
        { ICSharpCode.TextEditor.Document.IDocument doc = form.immediate.Document;
          doc.Insert(doc.TextLength, value);
          return;
        }
      }

      bool end = box.SelectionStart==box.TextLength;
      box.AppendText(value);
      if(end)
      { box.SelectionStart = box.TextLength;
        box.SelectionLength = 0;
      }
    }

    System.Windows.Forms.TextBox box;
  }

  System.Windows.Forms.TextBox textBox;

	#region Windows Form Designer generated code
	void InitializeComponent()
	{
    System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(OutputForm));
    this.textBox = new System.Windows.Forms.TextBox();
    this.SuspendLayout();
    // 
    // textBox
    // 
    this.textBox.Dock = System.Windows.Forms.DockStyle.Fill;
    this.textBox.Font = new System.Drawing.Font("Courier New", 9.75F, System.Drawing.FontStyle.Regular, System.Drawing.GraphicsUnit.Point, ((System.Byte)(0)));
    this.textBox.Location = new System.Drawing.Point(0, 0);
    this.textBox.Multiline = true;
    this.textBox.Name = "textBox";
    this.textBox.Size = new System.Drawing.Size(576, 197);
    this.textBox.TabIndex = 0;
    this.textBox.Text = "";
    // 
    // OutputForm
    // 
    this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
    this.ClientSize = new System.Drawing.Size(576, 197);
    this.Controls.Add(this.textBox);
    this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
    this.Name = "OutputForm";
    this.Text = "Console Output";
    this.ResumeLayout(false);
  }
	#endregion
	
  protected override void OnClosing(CancelEventArgs e)
  { Hide();
    e.Cancel = true;
    base.OnClosing(e);
  }

}

} // namespace NetLisp.IDE