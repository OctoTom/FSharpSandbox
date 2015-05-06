using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;

using System.Net;
using System.IO;

namespace CSharpThreading
{
  class Program
  {
    static void Main(string[] args)
    {
      Console.WriteLine("Main() begins.");
      //Test_Sequential();
      Test_Async();
      Console.WriteLine("Main() ends.");
      Console.ReadLine();
    }

    static void Test_Sequential()
    {
      var req = HttpWebRequest.Create("http://manning.com");
      var resp = req.GetResponse();
      var stream = resp.GetResponseStream();
      var reader = new StreamReader(stream);
      var html = reader.ReadToEnd();
      Console.WriteLine(html);
    }

    static void Test_Async()
    {
      var req = (HttpWebRequest)HttpWebRequest.Create("http://manning.com");
      req.BeginGetResponse(asyncRes1 => {
        var resp = req.EndGetResponse(asyncRes1);
        var stream = resp.GetResponseStream();
        var reader = new StreamReader(stream);
        var html = reader.ReadToEnd();
        Console.WriteLine(html);

        // Method BeginReadToEnd() does not exis but if it did it would be used like this.
        //reader.BeginReadToEnd(asyncRes2 => { 
        //  var html = reader.EndReadToEnd(asyncRes2);
        //  Console.WriteLine(html);
        //});
      }, null);
    }
  }
}
