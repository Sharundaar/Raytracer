using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Runtime.Serialization.Formatters.Binary;
using SharpDX;

namespace GraphEditor
{
    internal static class Program
    {
        private static void Main() => RaytracerProgram();

        private static void RaytracerProgram()
        {
            Raytracer.Raytracer tracer = new Raytracer.Raytracer();
            tracer.Init();
        }
    }
}