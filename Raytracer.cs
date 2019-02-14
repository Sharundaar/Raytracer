using System.Collections.Generic;
using System.IO;
using System.Windows.Forms;
using SharpDX;
using SharpDX.D3DCompiler;
using SharpDX.Direct3D;
using SharpDX.Direct3D11;
using SharpDX.DXGI;
using SharpDX.Windows;

using Format = SharpDX.DXGI.Format;
using SwapChainDescription  = SharpDX.DXGI.SwapChainDescription;

using Buffer = SharpDX.Direct3D11.Buffer;
using Device = SharpDX.Direct3D11.Device;
using System.Runtime.InteropServices;

namespace Raytracer
{
    public struct StandardBuffer
    {
        public StandardBuffer( int worldViewProj )
        {
            this.worldViewProj = worldViewProj;
        }
        public int worldViewProj;
    }

    public class Shader
    {
        public VertexShader vertexShader = null;
        public PixelShader pixelShader = null;
        public GeometryShader geometryShader = null;
        public InputLayout layout = null;

        public StandardBuffer vertexShaderSlot = new StandardBuffer( -1 );
        public StandardBuffer geomShaderSlot = new StandardBuffer( -1 );

        public bool validState = false;
    }

    public class ShaderBank
    {
        private struct ShaderReloadData
        {
            public string name;
            public string file;
            public InputElement[] inputElements;
            public System.Exception lastError;
        }

        private Device device;
        private Dictionary<string, Shader> shaders = new Dictionary<string, Shader>();
        private Dictionary<string, ShaderReloadData> shaderReloadData = new Dictionary<string, ShaderReloadData>();
        private Dictionary<string, FileSystemWatcher> fileWatchers = new Dictionary<string, FileSystemWatcher>();
        private List<ShaderReloadData> shaderReloadQueue = new List<ShaderReloadData>();

        public ShaderBank(Device device )
        {
            this.device = device;
        }

        private System.Tuple<Shader, System.Exception> LoadShader( string shaderFile, params InputElement[] inputElements )
        {
            var result = new Shader() {
                vertexShader = null,
                pixelShader = null,
                geometryShader = null,
                layout = null,
                validState = false
            };

            ShaderBytecode vertexShaderByteCode = null;
            ShaderBytecode pixelShaderByteCode = null;
            ShaderBytecode geometryShaderByteCode = null;
            ShaderSignature signature = null;
            VertexShader vertexShader = null;
            PixelShader pixelShader = null;
            GeometryShader geometryShader = null;
            InputLayout layout = null;

            System.Exception error = null;
            try
            {
                var shaderFileBytecode = File.ReadAllBytes( shaderFile );
                vertexShaderByteCode = ShaderBytecode.Compile(shaderFileBytecode, "VS", "vs_4_0", ShaderFlags.OptimizationLevel0);
                vertexShader = new VertexShader(device, vertexShaderByteCode);
                var reflection = new ShaderReflection(vertexShaderByteCode);
                /* Iterate through constant buffers */
                for( int i=0; i<reflection.Description.ConstantBuffers; ++i )
                {
                    var cb = reflection.GetConstantBuffer( i );
                    if( cb.Description.Name == "worldViewProj" )
                        result.vertexShaderSlot.worldViewProj = i;
                }
                reflection.Dispose();

                pixelShaderByteCode = ShaderBytecode.Compile(shaderFileBytecode, "PS", "ps_4_0", ShaderFlags.OptimizationLevel0);
                pixelShader = new PixelShader(device, pixelShaderByteCode);

                try
                {
                    geometryShaderByteCode = ShaderBytecode.Compile( shaderFileBytecode, "GS", "gs_4_0", ShaderFlags.OptimizationLevel0 );
                    geometryShader = new GeometryShader( device, geometryShaderByteCode );
                } 
                catch( CompilationException e )
                {
                    if( !e.Message.Contains( "'GS': entrypoint") )
                        throw e;
                }

                signature = ShaderSignature.GetInputSignature(vertexShaderByteCode);
                layout = new InputLayout(device, signature, inputElements);
                signature.Dispose(); 

                result.vertexShader = vertexShader;
                result.pixelShader = pixelShader;
                result.geometryShader = geometryShader;
                result.layout = layout;
            }
            catch( System.Exception e )
            {
                System.Console.WriteLine( "Error while compiling shader {0}:\n{1}", shaderFile, e );
                error = e;
            }
            finally
            {
                if( geometryShaderByteCode != null ) geometryShaderByteCode.Dispose();
                if( vertexShaderByteCode != null ) vertexShaderByteCode.Dispose();
                if( pixelShaderByteCode != null ) pixelShaderByteCode.Dispose();
                if( geometryShaderByteCode != null ) geometryShaderByteCode.Dispose();
                if( signature != null ) signature.Dispose();

                if( error!=null && vertexShader != null ) vertexShader.Dispose();
                if( error!=null && pixelShader != null ) pixelShader.Dispose();
                if( error!=null && geometryShader != null ) geometryShader.Dispose();
                if( error!=null && layout != null ) signature.Dispose();
            }

            result.validState = error == null;

            return System.Tuple.Create(result, error);
        }

        public void LoadShader( string name, string shaderFile, params InputElement[] inputElements )
        {
            var tpl = LoadShader( shaderFile, inputElements );
            var shader = tpl.Item1;
            var error = tpl.Item2;
            shaders[name] = shader;
            shaderReloadData[ Path.GetFileName(shaderFile) ] = new ShaderReloadData() {
                name = name,
                file = shaderFile,
                inputElements = inputElements,
                lastError = error
            };

            var directory = Path.GetDirectoryName( shaderFile );
            if( directory == string.Empty )
                directory = "./";
            if( !fileWatchers.ContainsKey( directory ) )
            {
                var watcher = new FileSystemWatcher( directory ) {
                    EnableRaisingEvents = true,
                };
                watcher.Changed += (sender, args) => {
                    ShaderReloadData reloadData;
                    if( shaderReloadData.TryGetValue( Path.GetFileName( args.FullPath ), out reloadData ) && !shaderReloadQueue.Contains( reloadData ) )
                        shaderReloadQueue.Add( reloadData );
                };
                fileWatchers[directory] = watcher;
            }
        }

        public bool ShaderNeedsReload()
        {
            return shaderReloadQueue.Count > 0;
        }

        public void ReloadQueuedShaders()
        {
            var reloadList = shaderReloadQueue.ToArray();
            shaderReloadQueue.Clear();
            foreach( var reloadData in reloadList )
            {
                var name = reloadData.name;
                var shader = shaders[name];
                Dispose( shader );
                LoadShader( name, reloadData.file, reloadData.inputElements );

                shader = shaders[name];
                var newReloadData = shaderReloadData[reloadData.file];
                if(shader.validState) System.Console.WriteLine( "Shader {0} reloaded successfully.", newReloadData.name );
                else if( newReloadData.lastError is System.IO.IOException ) shaderReloadQueue.Add( newReloadData );
            }
        }

        public Shader this[string name]
        {
            get
            {
                return shaders[name];
            }
        }

        public void Dispose( Shader shader )
        {
            if( shader.layout != null ) shader.layout.Dispose();
            if( shader.pixelShader != null ) shader.pixelShader.Dispose();
            if( shader.vertexShader != null ) shader.vertexShader.Dispose();
            if( shader.geometryShader != null ) shader.geometryShader.Dispose();
        }

        public void Dispose()
        {
            foreach( var shader in shaders.Values )
                Dispose( shader );
            shaders.Clear();
        }
    }

    public struct ImmediateVertex
    {
        public ImmediateVertex( Vector4 pos, Vector4 col ) { position = pos; color = col; uv = Vector2.Zero; }
        public ImmediateVertex( Vector4 pos, Vector4 col, Vector2 tex ) { position = pos; color = col; uv = tex; }
        public ImmediateVertex( Vector2 pos, Vector3 col ) { position = new Vector4( pos, 0, 1 ); color = new Vector4( col, 1 ); uv = Vector2.Zero; }
        public ImmediateVertex( Vector2 pos, Vector3 col, Vector2 tex ) { position = new Vector4( pos, 0, 1 ); color = new Vector4( col, 1 ); uv = tex; }
        public Vector4 position;
        public Vector4 color;
        public Vector2 uv;
    }

    [StructLayout(LayoutKind.Sequential)]
    public struct ImmediateConstantBuffer
    {
        public Matrix WVPMatrix;
        public Vector4 iResolution;
        public float iTime;
        public Vector3 CameraPos;
        public Matrix CameraDir;
    }

    public struct ImmediateTexture
    {
        public SamplerState sampler;
        public Texture2D texture;
        public ShaderResourceView view;
    }

    public class ImmediateContext
    {
        public static int MaxVertexCapacity = 2048;
        public static int ImmediateVertexSize = Utilities.SizeOf<ImmediateVertex>();

        public DeviceContext Context { get; private set; }

        private ImmediateVertex[] vertices = new ImmediateVertex[MaxVertexCapacity];
        private int allocatedVertices = 0;

        private Buffer vertexBuffer;
        private VertexBufferBinding vertexBufferBinding;

        private Buffer worldViewProjectionBuffer;

        private ImmediateTexture[] textures = new ImmediateTexture[5];

        public VertexBufferBinding VertexBufferBinding
        {
            get { return vertexBufferBinding; }
            set { vertexBufferBinding = value; }
        }
        public ImmediateConstantBuffer ConstantBuffer { get; set; }

        private Shader Shader { get; set; }
        public ShaderBank ShaderBank { get; set; }

        public PrimitiveTopology Topology { get; set; }

        public void SetShader( string name )
        {
            Shader = ShaderBank[name];
        }

        public void AddTexture( int slot, Texture2D texture )
        {
            System.Diagnostics.Debug.Assert( slot >= 0 && slot < textures.Length, string.Format( "Only {0} texture slots supported.", textures.Length ) );

            if( textures[slot].texture != null )
            {
                textures[slot].texture = null;
                textures[slot].view.Dispose();
            }

            textures[slot].texture = texture;
            textures[slot].view = new ShaderResourceView( texture.Device, texture );
        }

        public void Init(SharpDX.Direct3D11.Device device )
        {
            vertexBuffer = Buffer.Create( device, BindFlags.VertexBuffer, vertices, vertices.Length * ImmediateVertexSize, ResourceUsage.Dynamic, CpuAccessFlags.Write );
            vertexBufferBinding = new VertexBufferBinding() { Buffer = vertexBuffer, Offset = 0, Stride = ImmediateVertexSize };

            var cb = new ImmediateConstantBuffer()
            {
                WVPMatrix = Matrix.Identity,
                iResolution = Vector4.Zero,
                iTime = 0,
                CameraPos = Vector3.Zero,
                CameraDir = Matrix.Identity,
            };

            worldViewProjectionBuffer = Buffer.Create( device, BindFlags.ConstantBuffer, ref cb );
            this.ConstantBuffer = cb;
            
            for( int i=0; i<textures.Length; ++i )
            {
                textures[i].texture = null;
                textures[i].view = null;
                textures[i].sampler = new SamplerState( device, new SamplerStateDescription() {
                    Filter = Filter.MinMagMipLinear,
                    AddressU = TextureAddressMode.Clamp,
                    AddressV = TextureAddressMode.Clamp,
                    AddressW = TextureAddressMode.Clamp,
                    BorderColor = Color.Black,
                    ComparisonFunction = Comparison.Never,
                    MaximumAnisotropy = 16,
                    MipLodBias = 0,
                    MinimumLod = 0,
                    MaximumLod = float.MaxValue,
                } );
            }

            ShaderBank = new ShaderBank( device );
            ShaderBank.LoadShader( "raytracer", "raytracer.fx", 
                        new InputElement("POSITION", 0, Format.R32G32B32A32_Float, 0, 0) );

            Shader = ShaderBank["raytracer"];
            Context = device.ImmediateContext;
        }

        public void SetAllocatedVertices( int count ) // to be called in case we override the VertexBuffer so the context knows how many vertices to draw.
        {
            allocatedVertices = count;
        }

        public void Dispose()
        {
            ShaderBank.Dispose();

            for( int i=0; i<textures.Length; ++i )
            {
                textures[i].texture = null;
                if( textures[i].view != null ) textures[i].view.Dispose();
                textures[i].sampler.Dispose();
            }

            worldViewProjectionBuffer.Dispose();
            vertexBuffer.Dispose();

            worldViewProjectionBuffer = null;
            vertexBuffer = null;
            Context = null;
        }

        public void PartialClear()
        {
            allocatedVertices = 0;
        }

        public void Clear( bool onlyPartial = false )
        {
            PartialClear();
            if( onlyPartial )
                return;

            this.ConstantBuffer = new ImmediateConstantBuffer() {
                WVPMatrix = Matrix.Identity,
                iResolution = Vector4.Zero,
            };
            Shader = ShaderBank["raytracer"];
            Topology = PrimitiveTopology.TriangleList;

            for( int i=0; i<textures.Length; ++i )
            {
                if( textures[i].view != null )
                    textures[i].view.Dispose();
                textures[i].texture = null;
            }
        }

        public void Flush( bool partialClear = false )
        {
            if( allocatedVertices == 0 || !Shader.validState )
            {
                Clear( partialClear );
                return;
            }

            Context.InputAssembler.PrimitiveTopology = Topology;
            Context.InputAssembler.InputLayout = Shader.layout;
            if( Context.VertexShader.Get() != Shader.vertexShader ) Context.VertexShader.Set( Shader.vertexShader );
            if( Context.PixelShader.Get() != Shader.pixelShader ) Context.PixelShader.Set( Shader.pixelShader );
            if( Context.GeometryShader.Get() != Shader.geometryShader ) Context.GeometryShader.Set( Shader.geometryShader );

            if( vertexBufferBinding.Buffer == vertexBuffer ) // if we didn't override the buffer binding, update data
            {
                DataStream dataStream;
                Context.MapSubresource( vertexBuffer, MapMode.WriteDiscard, SharpDX.Direct3D11.MapFlags.None, out dataStream );
                dataStream.WriteRange(vertices, 0, allocatedVertices);
                Context.UnmapSubresource( vertexBuffer, 0 );
            }
            Context.InputAssembler.SetVertexBuffers( 0, vertexBufferBinding );
            
            var cb = this.ConstantBuffer;
            Context.UpdateSubresource( ref cb, worldViewProjectionBuffer );

            Context.VertexShader.SetConstantBuffers( 0, worldViewProjectionBuffer );
            Context.PixelShader.SetConstantBuffers( 0, worldViewProjectionBuffer );
            if( Shader.geometryShader != null )
                Context.GeometryShader.SetConstantBuffers( 0, worldViewProjectionBuffer );

            for( int slot = 0; slot<textures.Length; ++slot )
            {
                if( textures[slot].texture == null ) continue;
                Context.PixelShader.SetShaderResource( slot, textures[slot].view );
                Context.PixelShader.SetSampler( slot, textures[slot].sampler );
            }

            Context.Draw( allocatedVertices, 0 );

            Clear( partialClear );
        }
        public void DrawVertices( params ImmediateVertex[] vertices )
        {
            System.Array.Copy( vertices, 0, this.vertices, allocatedVertices, vertices.Length );
            allocatedVertices += vertices.Length;
        }

        public void DrawVertex( ImmediateVertex vertex )
        {
            this.vertices[allocatedVertices++] = vertex;
        }

        public void DrawRect( RectangleF rect, Color4 color )
        {
            DrawVertices(
                new ImmediateVertex( new Vector4(rect.Left, rect.Top, 0, 1), color, new Vector2( 0, 0 ) ),
                new ImmediateVertex( new Vector4(rect.Right, rect.Top, 0, 1), color, new Vector2( 1, 0 ) ),
                new ImmediateVertex( new Vector4(rect.Right, rect.Bottom, 0, 1), color, new Vector2( 1, 1 ) ),
                new ImmediateVertex( new Vector4(rect.Left, rect.Top, 0, 1), color, new Vector2( 0, 0 ) ),
                new ImmediateVertex( new Vector4(rect.Left, rect.Bottom, 0, 1), color, new Vector2( 0, 1 ) ),
                new ImmediateVertex( new Vector4(rect.Right, rect.Bottom, 0, 1), color, new Vector2( 1, 1 ) )
            );
        }
    }

    class Inputs
    {
        public enum MouseButton
        {
            Left = 0,
            Right,
            Middle,
            
            Count,
            None
        }

        public enum ModifierKey
        {
            Control = 0,
            Shift,
            Alt,

            Count,
            None
        }

        private bool[] modifierKeyDown = new bool[(uint)ModifierKey.Count];

        private bool[] prevMouseDown = new bool[(uint)MouseButton.Count];
        private bool[] mouseDown = new bool[(uint)MouseButton.Count];
        private bool[] prevMouseDragged = new bool[(uint)MouseButton.Count];
        private bool[] mouseDragged = new bool[(uint)MouseButton.Count];

        private Vector2 previousMousePosition = Vector2.Zero;
        private Vector2 mousePosition = Vector2.Zero;

        public Vector2 MousePosition
        {
            get { return mousePosition; }
            set
            {
                mousePosition = value;
                for( int i=0; i<(uint)MouseButton.Count; ++i )
                {
                    if( mouseDown[i] && MousePositionDelta != Vector2.Zero ) mouseDragged[i] = true; 
                }
            }
        }

        public Vector2 PreviousMousePosition
        {
            get { return previousMousePosition; }
        }

        public Vector2 MousePositionDelta
        {
            get { return mousePosition - previousMousePosition; }
        }

        public bool IsModifierKeyDown( ModifierKey key )
        {
            return modifierKeyDown[(int)key];
        }

        public void SetModifierKeyDown( bool down, Keys sysKey )
        {
            ModifierKey key = ModifierKey.None;
            switch( sysKey )
            {
            case Keys.ControlKey: key = ModifierKey.Control; break;
            case Keys.Menu: key = ModifierKey.Alt; break;
            case Keys.ShiftKey: key = ModifierKey.Shift; break;
            }
            if( key != ModifierKey.None )
                modifierKeyDown[(int)key] = down;
        }

        public void SetModifierKeyDown( bool down, ModifierKey key )
        {
            modifierKeyDown[(int)key] = down;
        }

        public void SetMouseDown( bool down, MouseButtons sysButton )
        {
            MouseButton button = MouseButton.None;
            switch( sysButton )
            {
                case MouseButtons.Left:   button = MouseButton.Left; break; 
                case MouseButtons.Right:  button = MouseButton.Right; break;
                case MouseButtons.Middle: button = MouseButton.Middle; break;
            }
            if( button != MouseButton.None )
                SetMouseDown( down, button );
        }

        public void SetMouseDown( bool down, MouseButton button )
        {
            mouseDown[(int)button] = down;
        }

        public bool IsMousePressed( MouseButton button )
        {
            return !prevMouseDown[(int)button] && mouseDown[(int)button];
        }

        public bool IsMouseClicked( MouseButton button )
        {
            return prevMouseDown[(int)button] && !mouseDown[(int)button] && !mouseDragged[(int)button];
        }

        public bool IsMouseDown( MouseButton button )
        {
            return mouseDown[(int)button];
        }

        public bool IsMouseUp( MouseButton button )
        {
            return !mouseDown[(int)button];
        }

        public bool IsMouseDragged( MouseButton button )
        {
            return mouseDragged[(int)button];
        }

        public bool IsMouseDragBegin( MouseButton button )
        {
            return !prevMouseDragged[(int)button] && mouseDragged[(int)button];
        }

        public void ClearFrameData()
        {
            for( int i=0; i<(int)MouseButton.Count; ++i )
            {
                prevMouseDragged[i] = mouseDragged[i];
                if( !mouseDown[i] ) mouseDragged[i] = false;
                prevMouseDown[i] = mouseDown[i];
            }
            previousMousePosition = mousePosition;
        }
    }

    public class DocumentRenderContext
    {
        public Vector2 ViewportSize { get; set; }
        public bool ViewportDirty { get; set; }

        public bool HasMouseFocus { get; set; }
        public Vector2 LastMousePosition { get; set; }
        public Vector2 LastMouseWorldPosition { get; set; }

        public double deltaTime;
        public double elsapsedTime;
    }

    class Raytracer
    {
        private RenderForm form;
        private bool running = true;

        private SwapChainDescription swapChainDescription;

        private SharpDX.Direct3D11.Device device;
        private DeviceContext context;
        private SwapChain swapChain;
        private ImmediateContext immediateContext;
        private RenderLoop renderLoop = null;

        private Texture2D backBuffer = null;
        private RenderTargetView renderView = null;
        private Texture2D depthBuffer = null;
        private DepthStencilView depthView = null;

        private Buffer resolutionBuffer;

        public SharpDX.Direct3D11.Device Device { get { return device; } set { device = value; } }
        public DocumentRenderContext RenderContext { get; set; }

        public Raytracer() {}

        public Inputs Inputs { get; set; } = new Inputs();

        bool[] pressed = new bool[6];

        public void Init()
        {
            form = new RenderForm("Raytracer");

            form.FormClosing += (sender, args) => {
                running = false;
            };

            var prevMousePos = Vector2.Zero;
            form.KeyUp += (sender, args) => {
                if (args.KeyCode == Keys.Escape)
                    form.Close();
                if( args.Control || args.Alt || args.Shift )
                    Inputs.SetModifierKeyDown( false, args.KeyCode );

                if( args.KeyCode == Keys.W )
                    pressed[0] = false;
                if( args.KeyCode == Keys.A )
                    pressed[1] = false;
                if( args.KeyCode == Keys.S )
                    pressed[2] = false;
                if( args.KeyCode == Keys.D )
                    pressed[3] = false;
                if( args.KeyCode == Keys.Q )
                    pressed[4] = false;
                if( args.KeyCode == Keys.E )
                    pressed[5] = false;
            };

            // handle mouse drag
            form.MouseDown += (sender, args) => { 
                Inputs.SetMouseDown( true, args.Button );
                prevMousePos = new Vector2( args.X, args.Y ); 
            };
            form.MouseUp += (sender, args) => {
                Inputs.SetMouseDown( false, args.Button );
            };
            form.MouseMove += (sender, args) => {
                Inputs.MousePosition = new Vector2( args.X, args.Y );
            };

            form.KeyDown += (sender, args) => {
                if( args.Control || args.Alt || args.Shift )
                    Inputs.SetModifierKeyDown( true, args.KeyCode );

                if( args.KeyCode == Keys.W )
                    pressed[0] = true;
                if( args.KeyCode == Keys.A )
                    pressed[1] = true;
                if( args.KeyCode == Keys.S )
                    pressed[2] = true;
                if( args.KeyCode == Keys.D )
                    pressed[3] = true;
                if( args.KeyCode == Keys.Q )
                    pressed[4] = true;
                if( args.KeyCode == Keys.E )
                    pressed[5] = true;

                if( args.KeyCode == Keys.R )
                {
                    cameraPos = new Vector3(0, 1, 0);
                }
            };

            InitRenderer();

            form.Show();
            Loop();
        }

        private void InitRenderer()
        {
            swapChainDescription = new SwapChainDescription()
            {
                BufferCount = 1,
                ModeDescription = new ModeDescription(form.ClientSize.Width, form.ClientSize.Height, new Rational(60, 1), Format.R8G8B8A8_UNorm),
                IsWindowed = true,
                OutputHandle = form.Handle,
                SampleDescription = new SampleDescription(1, 0),
                SwapEffect = SwapEffect.Discard,
                Usage = Usage.RenderTargetOutput,
            };

            Device.CreateWithSwapChain(DriverType.Hardware, DeviceCreationFlags.Debug, swapChainDescription, out device, out swapChain);
            context = device.ImmediateContext;

            var factory = swapChain.GetParent<Factory>();
            factory.MakeWindowAssociation(form.Handle, WindowAssociationFlags.IgnoreAll);
            factory.Dispose();

            form.UserResized += (sender, args) =>
            {
                var sform = (RenderForm)sender;
                RenderContext.ViewportDirty = true;
                RenderContext.ViewportSize = new Vector2(sform.ClientSize.Width, sform.ClientSize.Height);
            };

            form.MouseLeave += (sender, args) => RenderContext.HasMouseFocus = false;
            form.MouseEnter += (sender, args) => RenderContext.HasMouseFocus = true;
            form.GotFocus += (sender, args) => RenderContext.HasMouseFocus = true;
            form.LostFocus += (sender, args) => RenderContext.HasMouseFocus = false;
            form.MouseMove += (sender, args) => {
                /*
                RenderContext.LastMousePosition = new Vector2( args.X, args.Y );
                RenderContext.LastMouseWorldPosition = new Vector2( args.X, args.Y );
                RenderContext.LastMouseWorldPosition -= RenderContext.ViewportSize / 2.0f;
                var mouseWorldPosTransformed = new Vector4( RenderContext.LastMouseWorldPosition.X, RenderContext.LastMouseWorldPosition.Y, 0, 1 );
                RenderContext.LastMouseWorldPosition = new Vector2(mouseWorldPosTransformed.X, mouseWorldPosTransformed.Y);
                */
            };

            RenderContext = new DocumentRenderContext();
            RenderContext.ViewportSize = new Vector2(form.ClientSize.Width, form.ClientSize.Height);
            RenderContext.ViewportDirty = true;

            immediateContext = new ImmediateContext();
            immediateContext.Init( device );

            renderLoop = new RenderLoop( form );
        }

        private void SetupBuffers()
        {
            // Dispose all previous allocated resources
            Utilities.Dispose(ref backBuffer);
            Utilities.Dispose(ref renderView);
            Utilities.Dispose(ref depthBuffer);
            Utilities.Dispose(ref depthView);
            Utilities.Dispose(ref resolutionBuffer);

            // Resize the backbuffer
            swapChain.ResizeBuffers(swapChainDescription.BufferCount, (int)RenderContext.ViewportSize.X, (int)RenderContext.ViewportSize.Y, Format.Unknown, SwapChainFlags.None);

            // Get the backbuffer from the swapchain
            backBuffer = Texture2D.FromSwapChain<Texture2D>(swapChain, 0);

            // Renderview on the backbuffer
            renderView = new RenderTargetView(device, backBuffer);

            // Create the depth buffer
            depthBuffer = new Texture2D(device, new Texture2DDescription()
            {
                Format = Format.D32_Float_S8X24_UInt,
                ArraySize = 1,
                MipLevels = 1,
                Width = (int)RenderContext.ViewportSize.X,
                Height = (int)RenderContext.ViewportSize.Y,
                SampleDescription = new SampleDescription(1, 0),
                Usage = ResourceUsage.Default,
                BindFlags = BindFlags.DepthStencil,
                CpuAccessFlags = CpuAccessFlags.None,
                OptionFlags = ResourceOptionFlags.None
            });

            // Create the depth buffer view
            depthView = new DepthStencilView(device, depthBuffer );

            // Setup targets and viewport for rendering
            context.Rasterizer.State = new RasterizerState(device, new RasterizerStateDescription() {
                CullMode = CullMode.None,
                DepthBias = 0,
                DepthBiasClamp = 0,
                FillMode = FillMode.Solid,
                IsAntialiasedLineEnabled = false,
                IsDepthClipEnabled = true,
                IsFrontCounterClockwise = false,
                IsMultisampleEnabled = true,
                IsScissorEnabled = false,
                SlopeScaledDepthBias = 0
            });
            context.Rasterizer.SetViewport(new Viewport(0, 0, (int)RenderContext.ViewportSize.X, (int)RenderContext.ViewportSize.Y, 0.0f, 1.0f));
            context.OutputMerger.SetTargets(depthView, renderView);

            context.OutputMerger.SetDepthStencilState( new DepthStencilState( device, new DepthStencilStateDescription() {
                IsDepthEnabled = false,
            }), 0);

            Vector4 iResolution = new Vector4( RenderContext.ViewportSize, 0, 0 );
            resolutionBuffer = Buffer.Create( device, BindFlags.ConstantBuffer, ref iResolution );
            resolutionBuffer.DebugName = "ResolutionBuffer";
        }

        private Vector3 cameraPos = new Vector3(0, 1, 0);
        private Vector3 cameraDir = new Vector3(0, 0, 1);

        private Vector3 targetCamVelocity = Vector3.Zero;
        private Vector3 cameraVelocity = Vector3.Zero;

        float targetYaw = 0;
        float yaw = 0;
        float targetPitch = 0;
        float pitch = 0;


        private void Render()
        {
            if (RenderContext.ViewportDirty)
            {
                SetupBuffers();
                RenderContext.ViewportDirty = false;
            }

            var projMatrix = Matrix.OrthoLH(RenderContext.ViewportSize.X, -RenderContext.ViewportSize.Y, 0.1f, 100.0f);
            context.ClearDepthStencilView(depthView, DepthStencilClearFlags.Depth, 1.0f, 0);
            context.ClearRenderTargetView(renderView, new Color( 0.0f ));

            var cforward = cameraDir;
            cforward.Normalize();

            if( Inputs.IsMouseDown(Inputs.MouseButton.Left) )
            {
                targetYaw +=  Inputs.MousePositionDelta.X * (float)RenderContext.deltaTime * 100.0f;
                targetPitch += Inputs.MousePositionDelta.Y * (float)RenderContext.deltaTime * 100.0f;

                targetPitch = MathUtil.Clamp( targetPitch, -80.0f, 80.0f );
            }

            yaw = MathUtil.Lerp( yaw, targetYaw, .01f );
            pitch = MathUtil.Lerp( pitch, targetPitch, .01f );

            cforward = Vector3.Transform( cforward, Matrix3x3.RotationY( MathUtil.DegreesToRadians( yaw ) ) );

            var cup = Vector3.Up;
            var cright = Vector3.Cross( cup, cforward );

            cforward = Vector3.Transform( cforward, Matrix3x3.RotationAxis( cright, MathUtil.DegreesToRadians( pitch ) ) );
            cright = Vector3.Cross( cup, cforward );
            cup = Vector3.Cross(cforward, cright);

            cforward.Normalize();
            cup.Normalize();
            cright.Normalize();

            float speed = 5.0f;

            if( pressed[0] )
                targetCamVelocity.Z = speed;
            else if( pressed[2] )
                targetCamVelocity.Z = -speed;
            else
                targetCamVelocity.Z = 0;

            if( pressed[1] )
                targetCamVelocity.X = -speed;
            else if( pressed[3] )
                targetCamVelocity.X = speed;
            else
                targetCamVelocity.X = 0;

            if( pressed[4] )
                targetCamVelocity.Y = -speed;
            else if( pressed[5] )
                targetCamVelocity.Y = speed;
            else
                targetCamVelocity.Y = 0;

            cameraVelocity = cameraVelocity + 0.01f * (targetCamVelocity - cameraVelocity);
            var move = cforward * cameraVelocity.Z + cup * cameraVelocity.Y + cright * cameraVelocity.X;
            move *= (float)RenderContext.deltaTime;
            cameraPos += move;

            immediateContext.SetShader( "raytracer" );
            immediateContext.ConstantBuffer = new ImmediateConstantBuffer() {
                WVPMatrix = projMatrix,
                iResolution = new Vector4( RenderContext.ViewportSize, 0, 0 ),
                iTime = (float)RenderContext.elsapsedTime,
                CameraPos = cameraPos,
                CameraDir = new Matrix( cright.X, cup.X, cforward.X, 0, 
                                        cright.Y, cup.Y, cforward.Y, 0, 
                                        cright.Z, cup.Z, cforward.Z, 0,
                                        0, 0, 0, 0 ),
            };
            immediateContext.Topology = PrimitiveTopology.TriangleStrip;
            float left = -RenderContext.ViewportSize.X / 2.0f;
            float right = -left;
            float top = -RenderContext.ViewportSize.Y / 2.0f;
            float bottom = -top;

            immediateContext.DrawVertices(
                new ImmediateVertex( new Vector4( left, top, 0, 1 ), Vector4.Zero ),
                new ImmediateVertex( new Vector4( right, top, 0, 1 ), Vector4.Zero ),
                new ImmediateVertex( new Vector4( left, bottom, 0, 1 ), Vector4.Zero ),
                new ImmediateVertex( new Vector4( right, bottom, 0, 1 ), Vector4.Zero )
            );
            immediateContext.Flush();

            swapChain.Present(0, PresentFlags.None);

            Inputs.ClearFrameData();
        }

        private void Loop()
        {
            var frameCounter = new System.Diagnostics.Stopwatch();
            var timer = new System.Diagnostics.Stopwatch();
            int frameCounted = 0;
            System.TimeSpan elapsedTime = System.TimeSpan.Zero;

            timer.Start();
            while( running )
            {
                frameCounter.Restart();
                double newTime = timer.Elapsed.TotalSeconds;
                RenderContext.deltaTime = newTime - RenderContext.elsapsedTime;
                RenderContext.elsapsedTime = newTime;
                if( immediateContext.ShaderBank.ShaderNeedsReload() )
                    immediateContext.ShaderBank.ReloadQueuedShaders();
                if( renderLoop.NextFrame() )
                    Render();
                frameCounter.Stop();

                frameCounted++;
                elapsedTime += frameCounter.Elapsed;
                if( frameCounted >= 60 )
                {
                    form.Text = string.Format( "Raytracer - Framerate: {0}fps", System.Math.Floor( 1000.0 / (elapsedTime.TotalMilliseconds / frameCounted) ) );
                    elapsedTime = System.TimeSpan.Zero;
                    frameCounted = 0;
                }
            }
        }
    }
}