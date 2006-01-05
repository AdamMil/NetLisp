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
public class ExceptionForm : Form
{
  System.Windows.Forms.Label lblWhoops;
  System.Windows.Forms.Button btnOK;
  System.Windows.Forms.Button btnReport;
  System.Windows.Forms.TextBox txtReport;
  System.Windows.Forms.Button btnSend;
  System.Windows.Forms.TextBox txtException;

  public ExceptionForm()
	{ InitializeComponent();
	}
	
	public ExceptionForm(Exception e) : this()
	{ txtException.Text = e.ToString();
	}

	#region Windows Form Designer generated code
	void InitializeComponent()
	{
    System.Resources.ResourceManager resources = new System.Resources.ResourceManager(typeof(ExceptionForm));
    this.lblWhoops = new System.Windows.Forms.Label();
    this.txtException = new System.Windows.Forms.TextBox();
    this.btnOK = new System.Windows.Forms.Button();
    this.btnReport = new System.Windows.Forms.Button();
    this.txtReport = new System.Windows.Forms.TextBox();
    this.btnSend = new System.Windows.Forms.Button();
    this.SuspendLayout();
    // 
    // lblWhoops
    // 
    this.lblWhoops.BorderStyle = System.Windows.Forms.BorderStyle.FixedSingle;
    this.lblWhoops.Location = new System.Drawing.Point(4, 4);
    this.lblWhoops.Name = "lblWhoops";
    this.lblWhoops.Size = new System.Drawing.Size(384, 44);
    this.lblWhoops.TabIndex = 0;
    this.lblWhoops.Text = "An unhandled exception has occurred. It is recommended that you save your work an" +
      "d restart the IDE. The details of the exception are given below. Please consider" +
      " filling out a bug report to help me fix bugs in the IDE.";
    // 
    // txtException
    // 
    this.txtException.Location = new System.Drawing.Point(4, 56);
    this.txtException.Multiline = true;
    this.txtException.Name = "txtException";
    this.txtException.ReadOnly = true;
    this.txtException.Size = new System.Drawing.Size(384, 152);
    this.txtException.TabIndex = 1;
    this.txtException.Text = "";
    // 
    // btnOK
    // 
    this.btnOK.DialogResult = System.Windows.Forms.DialogResult.OK;
    this.btnOK.Location = new System.Drawing.Point(4, 216);
    this.btnOK.Name = "btnOK";
    this.btnOK.Size = new System.Drawing.Size(60, 23);
    this.btnOK.TabIndex = 2;
    this.btnOK.Text = "&Ok";
    // 
    // btnReport
    // 
    this.btnReport.DialogResult = System.Windows.Forms.DialogResult.OK;
    this.btnReport.Location = new System.Drawing.Point(72, 216);
    this.btnReport.Name = "btnReport";
    this.btnReport.Size = new System.Drawing.Size(120, 23);
    this.btnReport.TabIndex = 3;
    this.btnReport.Text = "Send bug report >>>";
    this.btnReport.Click += new System.EventHandler(this.btnReport_Click);
    // 
    // txtReport
    // 
    this.txtReport.Enabled = false;
    this.txtReport.Location = new System.Drawing.Point(4, 248);
    this.txtReport.Multiline = true;
    this.txtReport.Name = "txtReport";
    this.txtReport.Size = new System.Drawing.Size(384, 160);
    this.txtReport.TabIndex = 4;
    this.txtReport.Text = @"Please provide as much detail as possible.

1. What were you doing when the exception occurred?

2. Are you able to reproduce this exception? If so, what are the steps?

3. If you don't mind, please provide an email address so we can contact you with questions.";
    // 
    // btnSend
    // 
    this.btnSend.Enabled = false;
    this.btnSend.Location = new System.Drawing.Point(4, 416);
    this.btnSend.Name = "btnSend";
    this.btnSend.Size = new System.Drawing.Size(60, 23);
    this.btnSend.TabIndex = 5;
    this.btnSend.Text = "&Send";
    this.btnSend.Click += new System.EventHandler(this.btnSend_Click);
    // 
    // ExceptionForm
    // 
    this.AutoScaleBaseSize = new System.Drawing.Size(5, 13);
    this.ClientSize = new System.Drawing.Size(392, 247);
    this.Controls.Add(this.btnSend);
    this.Controls.Add(this.txtReport);
    this.Controls.Add(this.btnReport);
    this.Controls.Add(this.btnOK);
    this.Controls.Add(this.txtException);
    this.Controls.Add(this.lblWhoops);
    this.FormBorderStyle = System.Windows.Forms.FormBorderStyle.FixedDialog;
    this.Icon = ((System.Drawing.Icon)(resources.GetObject("$this.Icon")));
    this.Name = "ExceptionForm";
    this.StartPosition = System.Windows.Forms.FormStartPosition.CenterScreen;
    this.Text = "Unhandled exception";
    this.ResumeLayout(false);

  }
	#endregion

  void btnReport_Click(object sender, System.EventArgs e)
  { btnReport.Enabled = false;
    txtReport.Enabled = btnSend.Enabled = true;
    txtReport.Focus();

    Rectangle bounds = Bounds;
    bounds.Height += 200;
    bounds.Y      = (Screen.FromControl(this).WorkingArea.Height-bounds.Height)/2;
    Bounds = bounds;
  }

  void btnSend_Click(object sender, System.EventArgs e)
  { btnSend.Enabled = txtReport.Enabled = false;
    Height -= 200;
  }
}

} // namespace NetLisp.IDE
