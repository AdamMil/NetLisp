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
using System.Collections;
using System.ComponentModel;
using System.Windows.Forms;

namespace NetLisp.IDE
{

public class MainForm : Form
{
  System.Windows.Forms.MainMenu menuBar;
  System.Windows.Forms.MenuItem menuFile;
  System.Windows.Forms.MenuItem menuSaveAs;
  System.Windows.Forms.MenuItem menuSep1;
  System.Windows.Forms.MenuItem menuWindow;
  System.Windows.Forms.MenuItem menuNew;
  System.Windows.Forms.MenuItem menuOpen;
  System.Windows.Forms.MenuItem menuClose;
  System.Windows.Forms.MenuItem menuSave;
  System.Windows.Forms.MenuItem menuExit;
  System.Windows.Forms.MenuItem menuDebug;
  System.Windows.Forms.MenuItem menuExamine;
  System.Windows.Forms.MenuItem menuSep2;
  System.Windows.Forms.MenuItem menuCascade;
  System.Windows.Forms.MenuItem menuHorz;
  System.Windows.Forms.MenuItem menuVert;
  System.Windows.Forms.MenuItem menuWindowOutput;
  System.Windows.Forms.MenuItem menuCompile;
  System.Windows.Forms.MenuItem menuEdit;
  System.Windows.Forms.MenuItem menuUndo;
  System.Windows.Forms.MenuItem menuRedo;
  System.Windows.Forms.MenuItem menuGotoLine;
  System.Windows.Forms.MenuItem menuFind;
  System.Windows.Forms.MenuItem menuFindNext;
  System.Windows.Forms.MenuItem menuRun;
  System.Windows.Forms.MenuItem menuSep4;
  System.Windows.Forms.MenuItem menuSep3;
  System.Windows.Forms.MenuItem menuAutoChdir;
  private System.Windows.Forms.MenuItem menuRedirectStdout;
  OutputForm outputForm;

	public MainForm()
	{ InitializeComponent();
		outputForm = new OutputForm();
	}

  public bool AutoChdir { get { return menuAutoChdir.Checked; } }
  public bool RedirectStdout { get { return menuRedirectStdout.Checked; } }

	#region Windows Form Designer generated code
	void InitializeComponent()
  {
    System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(MainForm));
    this.menuBar = new System.Windows.Forms.MainMenu();
    this.menuFile = new System.Windows.Forms.MenuItem();
    this.menuNew = new System.Windows.Forms.MenuItem();
    this.menuOpen = new System.Windows.Forms.MenuItem();
    this.menuClose = new System.Windows.Forms.MenuItem();
    this.menuCompile = new System.Windows.Forms.MenuItem();
    this.menuSave = new System.Windows.Forms.MenuItem();
    this.menuSaveAs = new System.Windows.Forms.MenuItem();
    this.menuSep1 = new System.Windows.Forms.MenuItem();
    this.menuExit = new System.Windows.Forms.MenuItem();
    this.menuEdit = new System.Windows.Forms.MenuItem();
    this.menuUndo = new System.Windows.Forms.MenuItem();
    this.menuRedo = new System.Windows.Forms.MenuItem();
    this.menuSep4 = new System.Windows.Forms.MenuItem();
    this.menuGotoLine = new System.Windows.Forms.MenuItem();
    this.menuFind = new System.Windows.Forms.MenuItem();
    this.menuFindNext = new System.Windows.Forms.MenuItem();
    this.menuDebug = new System.Windows.Forms.MenuItem();
    this.menuExamine = new System.Windows.Forms.MenuItem();
    this.menuRun = new System.Windows.Forms.MenuItem();
    this.menuSep3 = new System.Windows.Forms.MenuItem();
    this.menuAutoChdir = new System.Windows.Forms.MenuItem();
    this.menuWindow = new System.Windows.Forms.MenuItem();
    this.menuWindowOutput = new System.Windows.Forms.MenuItem();
    this.menuSep2 = new System.Windows.Forms.MenuItem();
    this.menuCascade = new System.Windows.Forms.MenuItem();
    this.menuHorz = new System.Windows.Forms.MenuItem();
    this.menuVert = new System.Windows.Forms.MenuItem();
    this.menuRedirectStdout = new System.Windows.Forms.MenuItem();
    // 
    // menuBar
    // 
    this.menuBar.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                          this.menuFile,
                                                                          this.menuEdit,
                                                                          this.menuDebug,
                                                                          this.menuWindow});
    // 
    // menuFile
    // 
    this.menuFile.Index = 0;
    this.menuFile.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                            this.menuNew,
                                                                            this.menuOpen,
                                                                            this.menuClose,
                                                                            this.menuCompile,
                                                                            this.menuSave,
                                                                            this.menuSaveAs,
                                                                            this.menuSep1,
                                                                            this.menuExit});
    this.menuFile.Text = "&File";
    this.menuFile.Popup += new System.EventHandler(this.menuFile_Popup);
    // 
    // menuNew
    // 
    this.menuNew.Index = 0;
    this.menuNew.Shortcut = System.Windows.Forms.Shortcut.CtrlN;
    this.menuNew.Text = "&New";
    this.menuNew.Click += new System.EventHandler(this.menuNew_Click);
    // 
    // menuOpen
    // 
    this.menuOpen.Index = 1;
    this.menuOpen.Shortcut = System.Windows.Forms.Shortcut.CtrlO;
    this.menuOpen.Text = "&Open...";
    this.menuOpen.Click += new System.EventHandler(this.menuOpen_Click);
    // 
    // menuClose
    // 
    this.menuClose.Index = 2;
    this.menuClose.Text = "Close";
    this.menuClose.Click += new System.EventHandler(this.menuClose_Click);
    // 
    // menuCompile
    // 
    this.menuCompile.Index = 3;
    this.menuCompile.Text = "&Compile...";
    // 
    // menuSave
    // 
    this.menuSave.Index = 4;
    this.menuSave.Shortcut = System.Windows.Forms.Shortcut.CtrlS;
    this.menuSave.Text = "&Save";
    this.menuSave.Click += new System.EventHandler(this.menuSave_Click);
    // 
    // menuSaveAs
    // 
    this.menuSaveAs.Index = 5;
    this.menuSaveAs.Text = "Save &As...";
    this.menuSaveAs.Click += new System.EventHandler(this.menuSaveAs_Click);
    // 
    // menuSep1
    // 
    this.menuSep1.Index = 6;
    this.menuSep1.Text = "-";
    // 
    // menuExit
    // 
    this.menuExit.Index = 7;
    this.menuExit.Text = "E&xit";
    this.menuExit.Click += new System.EventHandler(this.menuExit_Click);
    // 
    // menuEdit
    // 
    this.menuEdit.Index = 1;
    this.menuEdit.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                            this.menuUndo,
                                                                            this.menuRedo,
                                                                            this.menuSep4,
                                                                            this.menuGotoLine,
                                                                            this.menuFind,
                                                                            this.menuFindNext});
    this.menuEdit.Text = "&Edit";
    this.menuEdit.Popup += new System.EventHandler(this.menuEdit_Popup);
    // 
    // menuUndo
    // 
    this.menuUndo.Index = 0;
    this.menuUndo.Shortcut = System.Windows.Forms.Shortcut.CtrlZ;
    this.menuUndo.Text = "&Undo";
    this.menuUndo.Click += new System.EventHandler(this.menuUndo_Click);
    // 
    // menuRedo
    // 
    this.menuRedo.Index = 1;
    this.menuRedo.Shortcut = System.Windows.Forms.Shortcut.CtrlY;
    this.menuRedo.Text = "&Redo";
    this.menuRedo.Click += new System.EventHandler(this.menuRedo_Click);
    // 
    // menuSep4
    // 
    this.menuSep4.Index = 2;
    this.menuSep4.Text = "-";
    // 
    // menuGotoLine
    // 
    this.menuGotoLine.Index = 3;
    this.menuGotoLine.Shortcut = System.Windows.Forms.Shortcut.CtrlG;
    this.menuGotoLine.Text = "&Go to Line...";
    this.menuGotoLine.Click += new System.EventHandler(this.menuGotoLine_Click);
    // 
    // menuFind
    // 
    this.menuFind.Index = 4;
    this.menuFind.Shortcut = System.Windows.Forms.Shortcut.CtrlF;
    this.menuFind.Text = "&Find...";
    // 
    // menuFindNext
    // 
    this.menuFindNext.Index = 5;
    this.menuFindNext.Shortcut = System.Windows.Forms.Shortcut.F3;
    this.menuFindNext.Text = "Find &Next";
    // 
    // menuDebug
    // 
    this.menuDebug.Index = 2;
    this.menuDebug.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                            this.menuExamine,
                                                                            this.menuRun,
                                                                            this.menuSep3,
                                                                            this.menuAutoChdir,
                                                                            this.menuRedirectStdout});
    this.menuDebug.MergeOrder = 1;
    this.menuDebug.MergeType = System.Windows.Forms.MenuMerge.MergeItems;
    this.menuDebug.Text = "&Debug";
    this.menuDebug.Popup += new System.EventHandler(this.menuDebug_Popup);
    // 
    // menuExamine
    // 
    this.menuExamine.Index = 0;
    this.menuExamine.Text = "&Examine Object";
    // 
    // menuRun
    // 
    this.menuRun.Index = 1;
    this.menuRun.Shortcut = System.Windows.Forms.Shortcut.F5;
    this.menuRun.Text = "&Run file";
    this.menuRun.Click += new System.EventHandler(this.menuRun_Click);
    // 
    // menuSep3
    // 
    this.menuSep3.Index = 2;
    this.menuSep3.Text = "-";
    // 
    // menuAutoChdir
    // 
    this.menuAutoChdir.Checked = true;
    this.menuAutoChdir.Index = 3;
    this.menuAutoChdir.Text = "Auto chdir to file directory on Run";
    this.menuAutoChdir.Click += new System.EventHandler(this.menuToggleCheck_Click);
    // 
    // menuWindow
    // 
    this.menuWindow.Index = 3;
    this.menuWindow.MenuItems.AddRange(new System.Windows.Forms.MenuItem[] {
                                                                              this.menuWindowOutput,
                                                                              this.menuSep2,
                                                                              this.menuCascade,
                                                                              this.menuHorz,
                                                                              this.menuVert});
    this.menuWindow.MergeOrder = 2;
    this.menuWindow.Text = "&Window";
    this.menuWindow.Popup += new System.EventHandler(this.menuWindow_Popup);
    // 
    // menuWindowOutput
    // 
    this.menuWindowOutput.Index = 0;
    this.menuWindowOutput.Shortcut = System.Windows.Forms.Shortcut.CtrlShiftO;
    this.menuWindowOutput.Text = "Show &Output Window";
    this.menuWindowOutput.Click += new System.EventHandler(this.menuWindowOutput_Click);
    // 
    // menuSep2
    // 
    this.menuSep2.Index = 1;
    this.menuSep2.Text = "-";
    // 
    // menuCascade
    // 
    this.menuCascade.Index = 2;
    this.menuCascade.Text = "Cascade";
    this.menuCascade.Click += new System.EventHandler(this.menuCascade_Click);
    // 
    // menuHorz
    // 
    this.menuHorz.Index = 3;
    this.menuHorz.Text = "Tile Horizontally";
    this.menuHorz.Click += new System.EventHandler(this.menuHorz_Click);
    // 
    // menuVert
    // 
    this.menuVert.Index = 4;
    this.menuVert.Text = "Tile Vertically";
    this.menuVert.Click += new System.EventHandler(this.menuVert_Click);
    // 
    // menuRedirectStdout
    // 
    this.menuRedirectStdout.Index = 4;
    this.menuRedirectStdout.Text = "Redirect stdout to immediate";
    this.menuRedirectStdout.Click += new System.EventHandler(this.menuToggleCheck_Click);
    // 
    // MainForm
    // 
    this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
    this.ClientSize = new System.Drawing.Size(612, 393);
    this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
    this.IsMdiContainer = true;
    this.Menu = this.menuBar;
    this.Name = "MainForm";
    this.Text = "NetLisp IDE";
    this.WindowState = System.Windows.Forms.FormWindowState.Maximized;

  }
	#endregion

  void menuFile_Popup(object sender, EventArgs e)
  { menuClose.Enabled = menuCompile.Enabled = menuSave.Enabled = menuSaveAs.Enabled = ActiveMdiChild is EditForm;
  }

  void menuDebug_Popup(object sender, EventArgs e)
  { menuExamine.Enabled = menuRun.Enabled = ActiveMdiChild is EditForm;
  }

  void menuClose_Click(object sender, System.EventArgs e)
  { EditForm form = ActiveMdiChild as EditForm;
    if(form!=null) form.Close();
  }

  void menuNew_Click(object sender, System.EventArgs e)
  { EditForm form = new EditForm();
    form.MdiParent = this;
    form.Show();
  }

  void menuOpen_Click(object sender, System.EventArgs e)
  { EditForm form = ActiveMdiChild as EditForm;
    if(form!=null && form.Filename==null) form=null;

    OpenFileDialog fd = new OpenFileDialog();
    fd.DefaultExt = ".lisp";
    fd.Filter = "Lisp files (*.lisp)|*.lisp|Scheme files (*.scm)|*.scm|All files (*.*)|*.*";
    if(form!=null) fd.InitialDirectory = System.IO.Path.GetDirectoryName(form.Filename);
    fd.RestoreDirectory = true;
    fd.Title = "Select source file...";
    if(fd.ShowDialog()==DialogResult.OK)
    { form = new EditForm();
      form.Load(fd.FileName);
      form.MdiParent = this;
      form.Show();
    }
  }

  void menuSave_Click(object sender, System.EventArgs e)
  { EditForm form = ActiveMdiChild as EditForm;
    if(form!=null) form.Save();
  }

  void menuSaveAs_Click(object sender, System.EventArgs e)
  { EditForm form = ActiveMdiChild as EditForm;
    if(form!=null) form.SaveAs();
  }

  void menuCascade_Click(object sender, System.EventArgs e) { LayoutMdi(MdiLayout.Cascade); }
  void menuHorz_Click(object sender, System.EventArgs e) { LayoutMdi(MdiLayout.TileHorizontal); }
  void menuVert_Click(object sender, System.EventArgs e) { LayoutMdi(MdiLayout.TileVertical); }

  void menuWindowOutput_Click(object sender, System.EventArgs e)
  { if(outputForm.MdiParent==null)
    { outputForm.MdiParent = this;
      outputForm.Show();
    }
    else outputForm.Show();
  }

  void menuExit_Click(object sender, System.EventArgs e) { Close(); }

  void menuEdit_Popup(object sender, EventArgs e)
  { Form form = ActiveMdiChild;
    Control ctl = form==null ? null : form.ActiveControl;
    TextBoxBase text = ctl as TextBoxBase;
    LispBox rich = ctl as LispBox;

    menuRedo.Enabled = rich!=null && rich.Document.UndoStack.CanRedo;
    menuUndo.Enabled = rich!=null && rich.Document.UndoStack.CanUndo || text!=null && text.CanUndo;
    menuGotoLine.Enabled = menuFind.Enabled = rich!=null || text!=null;
    
    menuFindNext.Enabled = false;
  }

  void menuUndo_Click(object sender, System.EventArgs e)
  { Form form = ActiveMdiChild;
    Control ctl = form==null ? null : form.ActiveControl;
    if(ctl is LispBox) ((LispBox)ctl).Undo();
    else if(ctl is TextBoxBase) ((TextBoxBase)ctl).Undo();
  }

  void menuRedo_Click(object sender, System.EventArgs e)
  { Form form = ActiveMdiChild;
    LispBox ctl = form==null ? null : form.ActiveControl as LispBox;
    if(ctl!=null) ctl.Redo();
  }

  void menuGotoLine_Click(object sender, System.EventArgs e)
  { Form form = ActiveMdiChild;
    Control ctl = form==null ? null : form.ActiveControl;
    if(ctl!=null)
    { GotoLineForm gl = new GotoLineForm();
      if(gl.ShowDialog()==DialogResult.OK)
      { int line = gl.Line;
        if(line<0) return;

        if(ctl is LispBox)
        { LispBox box = (LispBox)ctl;
          box.ActiveTextAreaControl.Caret.Line = Math.Max(1, Math.Min(line, box.Document.TotalNumberOfLines))-1;
        }
        else if(ctl is TextBoxBase)
        { TextBoxBase box = (TextBoxBase)ctl;
          string[] lines = App.GetRawLines(box);
          int pos=0;
          line = Math.Max(1, Math.Min(line, lines.Length))-1;
          for(int i=0; i<line; i++) pos += lines[i].Length;
          box.SelectionStart  = pos;
          box.SelectionLength = 0;
        }
      }
    }
  }

  void menuRun_Click(object sender, System.EventArgs e)
  { EditForm form = ActiveMdiChild as EditForm;
    if(form!=null) form.Run();
  }

  void menuWindow_Popup(object sender, EventArgs e)
  { menuHorz.Enabled = menuVert.Enabled = menuCascade.Enabled = MdiChildren.Length!=0;
  }

  void menuToggleCheck_Click(object sender, System.EventArgs e)
  { MenuItem item = (MenuItem)sender;
    item.Checked = !item.Checked;
  }
}

} // namespace NetLisp.IDE
