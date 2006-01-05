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
using System.Drawing;
using System.IO;
using System.ComponentModel;
using System.Windows.Forms;
using NetLisp.Backend;

namespace NetLisp.IDE
{

public class EditForm : System.Windows.Forms.Form
{ public EditForm()
	{ lispModule = new LispModule("edit_window");
    Builtins.Instance.Import(lispModule.TopLevel);

	  InitializeComponent();
	  edit.Focus();
	  edit.Document.DocumentChanged += new ICSharpCode.TextEditor.Document.DocumentEventHandler(Document_DocumentChanged);
	}

  public string Filename { get { return filename; } }
  public bool Modified { get { return modified; } }

  public new void Load(string path)
  { if(Modified && MessageBox.Show("The current document has been modified. Loading a file will discard these changes. Discard changes?",
                                   "Discard changes?", MessageBoxButtons.YesNo, MessageBoxIcon.Warning,
                                   MessageBoxDefaultButton.Button2) != DialogResult.Yes)
      return;

    edit.LoadFile(path, false);
    SetFilename(Path.GetFullPath(path));
    modified = false;
  }

  public void Run() { Run(edit.Text, false); }
  public void Run(string code, bool interactive)
  { if(code.Trim().Length==0) return;

    if(filename!=null && App.MainForm.AutoChdir) Environment.CurrentDirectory = Path.GetDirectoryName(filename);

    Backend.TopLevel old = Backend.TopLevel.Current;
    try
    { Backend.TopLevel.Current = lispModule.TopLevel;
      object ret = Builtins.eval.core(Parser.FromString(code).Parse());
      if(interactive) immediate.AppendLine(Ops.Repr(ret));
    }
    catch(Exception ex) { immediate.AppendLine("Error {0}: {1}", ex.GetType().Name, ex.Message); }
    finally { Backend.TopLevel.Current = old; }
  }

  public void Save()
  { if(filename==null) SaveAs();
    else
    { edit.SaveFile(filename);
      Text = Path.GetFileName(filename);
      modified = false;
    }
  }

  public void SaveAs()
  { SaveFileDialog fd = new SaveFileDialog();
    fd.DefaultExt = ".lisp";
    fd.Filter = "Lisp files (*.lisp)|*.lisp|Scheme files (*.scm)|*.scm|All files (*.*)|*.*";
    if(filename!=null) fd.InitialDirectory = Path.GetDirectoryName(filename);
    fd.RestoreDirectory = true;
    fd.Title = "Select destination file...";
    if(fd.ShowDialog()==DialogResult.OK)
    { SetFilename(fd.FileName);
      Save();
    }
  }

	#region Windows Form Designer generated code
	void InitializeComponent()
	{
    System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(EditForm));
    this.immediate = new NetLisp.IDE.ImmediateBox();
    this.edit = new NetLisp.IDE.LispBox();
    this.acbox = new NetLisp.IDE.AutoCompleteBox();
    this.lblImmediate = new System.Windows.Forms.Label();
    this.pnlCode = new System.Windows.Forms.Panel();
    this.pnlImmediate = new System.Windows.Forms.Panel();
    this.splitter = new System.Windows.Forms.Splitter();
    this.pnlCode.SuspendLayout();
    this.pnlImmediate.SuspendLayout();
    this.SuspendLayout();
    // 
    // immediate
    // 
    this.immediate.AllowDrop = true;
    this.immediate.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
      | System.Windows.Forms.AnchorStyles.Left) 
      | System.Windows.Forms.AnchorStyles.Right)));
    this.immediate.ConvertTabsToSpaces = true;
    this.immediate.EnableFolding = false;
    this.immediate.Encoding = ((System.Text.Encoding)(resources.GetObject("immediate.Encoding")));
    this.immediate.Location = new System.Drawing.Point(0, 16);
    this.immediate.Name = "immediate";
    this.immediate.ShowInvalidLines = false;
    this.immediate.ShowLineNumbers = false;
    this.immediate.Size = new System.Drawing.Size(656, 94);
    this.immediate.TabIndent = 2;
    this.immediate.TabIndex = 0;
    // 
    // edit
    // 
    this.edit.AllowDrop = true;
    this.edit.Anchor = ((System.Windows.Forms.AnchorStyles)((((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Bottom) 
      | System.Windows.Forms.AnchorStyles.Left) 
      | System.Windows.Forms.AnchorStyles.Right)));
    this.edit.ConvertTabsToSpaces = true;
    this.edit.EnableFolding = false;
    this.edit.Encoding = ((System.Text.Encoding)(resources.GetObject("edit.Encoding")));
    this.edit.Location = new System.Drawing.Point(0, 0);
    this.edit.Name = "edit";
    this.edit.ShowInvalidLines = false;
    this.edit.ShowLineNumbers = false;
    this.edit.Size = new System.Drawing.Size(656, 288);
    this.edit.TabIndent = 2;
    this.edit.TabIndex = 0;
    // 
    // acbox
    // 
    this.acbox.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
    this.acbox.Location = new System.Drawing.Point(144, 144);
    this.acbox.Name = "acbox";
    this.acbox.Size = new System.Drawing.Size(208, 106);
    this.acbox.TabIndex = 4;
    this.acbox.Visible = false;
    // 
    // lblImmediate
    // 
    this.lblImmediate.Anchor = ((System.Windows.Forms.AnchorStyles)(((System.Windows.Forms.AnchorStyles.Top | System.Windows.Forms.AnchorStyles.Left) 
      | System.Windows.Forms.AnchorStyles.Right)));
    this.lblImmediate.Location = new System.Drawing.Point(0, 0);
    this.lblImmediate.Name = "lblImmediate";
    this.lblImmediate.Size = new System.Drawing.Size(656, 16);
    this.lblImmediate.TabIndex = 2;
    this.lblImmediate.Text = "Immediate";
    // 
    // pnlCode
    // 
    this.pnlCode.Controls.Add(this.edit);
    this.pnlCode.Dock = System.Windows.Forms.DockStyle.Fill;
    this.pnlCode.Location = new System.Drawing.Point(0, 0);
    this.pnlCode.Name = "pnlCode";
    this.pnlCode.Size = new System.Drawing.Size(656, 293);
    this.pnlCode.TabIndex = 3;
    // 
    // pnlImmediate
    // 
    this.pnlImmediate.Controls.Add(this.lblImmediate);
    this.pnlImmediate.Controls.Add(this.immediate);
    this.pnlImmediate.Dock = System.Windows.Forms.DockStyle.Bottom;
    this.pnlImmediate.Location = new System.Drawing.Point(0, 293);
    this.pnlImmediate.Name = "pnlImmediate";
    this.pnlImmediate.Size = new System.Drawing.Size(656, 112);
    this.pnlImmediate.TabIndex = 3;
    // 
    // splitter
    // 
    this.splitter.Dock = System.Windows.Forms.DockStyle.Bottom;
    this.splitter.Location = new System.Drawing.Point(0, 290);
    this.splitter.Name = "splitter";
    this.splitter.Size = new System.Drawing.Size(656, 3);
    this.splitter.TabIndex = 5;
    this.splitter.TabStop = false;
    // 
    // EditForm
    // 
    this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
    this.ClientSize = new System.Drawing.Size(656, 405);
    this.Controls.Add(this.splitter);
    this.Controls.Add(this.pnlCode);
    this.Controls.Add(this.pnlImmediate);
    this.Controls.Add(this.acbox);
    this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
    this.KeyPreview = true;
    this.Name = "EditForm";
    this.Text = "New file";
    this.pnlCode.ResumeLayout(false);
    this.pnlImmediate.ResumeLayout(false);
    this.ResumeLayout(false);

  }
	#endregion

  protected override void OnClosing(CancelEventArgs e)
  { if(!e.Cancel && Modified)
    { DialogResult res = MessageBox.Show("This window has not been saved. Save before closing?",
                                         "Save "+(filename==null ? "file" : Path.GetFileName(filename))+"?",
                                         MessageBoxButtons.YesNoCancel, MessageBoxIcon.Warning);
      if(res==DialogResult.Cancel) e.Cancel=true;
      else if(res==DialogResult.Yes)
        try
        { Save();
          e.Cancel = Modified;
        }
        catch { e.Cancel=true; }
    }

    base.OnClosing(e);
  }

  protected override void OnKeyDown(KeyEventArgs e)
  { if(e.KeyData==Keys.F6)
    { if(edit.ContainsFocus) immediate.Focus();
      else edit.Focus();
      e.Handled = true;
    }
    base.OnKeyDown(e);
  }

  void SetFilename(string path)
  { filename = path;
    Text = Path.GetFileName(path);
  }

  void Document_DocumentChanged(object sender, ICSharpCode.TextEditor.Document.DocumentEventArgs e)
  { if(!modified)
    { modified = true;
      Text += "*";
    }
  }

  internal AutoCompleteBox acbox;
  internal LispModule lispModule;
  LispBox edit;
  internal ImmediateBox immediate;
  string filename;
  bool modified;

  System.Windows.Forms.Label lblImmediate;
  System.Windows.Forms.Panel pnlCode;
  System.Windows.Forms.Panel pnlImmediate;
  System.Windows.Forms.Splitter splitter;
}

} // namespace NetLisp.IDE
