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
using System.IO;
using System.Windows.Forms;

[assembly: System.Reflection.AssemblyKeyFile("../../../NetLisp.snk")]

namespace NetLisp.IDE
{

class App
{ public static readonly MainForm MainForm = new MainForm();

  public static string[] GetRawLines(TextBoxBase box)
  { ArrayList list = new ArrayList();
    string text = box.Text;
    int pos=0;
    while(pos<text.Length)
    { int index = text.IndexOf('\n', pos);
      if(index==-1) { list.Add(text.Substring(pos)); break; }
      else { list.Add(text.Substring(pos, index-pos+1)); pos=index+1; }
    }
    return (string[])list.ToArray(typeof(string));
  }

  [STAThread]
  static void Main()
  { NetLisp.Backend.Options.Debug = true;
    NetLisp.Backend.Options.Optimize = true;

    Application.ThreadException += new System.Threading.ThreadExceptionEventHandler(Application_ThreadException);
    Application.Run(MainForm);
    
    NetLisp.Backend.SnippetMaker.DumpAssembly();
  }

  static void Application_ThreadException(object sender, System.Threading.ThreadExceptionEventArgs e)
  { ExceptionForm form = new ExceptionForm(e.Exception);
    form.ShowDialog();
  }
}

} // namespace NetLisp.IDE