
struct VS_IN
{
	float4 pos : POSITION;
};

struct PS_IN
{
	float4 pos : SV_POSITION;
};

float4x4 worldViewProj;
float4 iResolution;
float iTime;
float3 CameraPos;
float4x4 CameraDir;

float4 VS( VS_IN input ) : SV_Position
{
	return mul(input.pos, worldViewProj);
}

#define MAX_STEPS 100
#define MAX_DIST 10000.
#define SURF_DIST .01
#define L2Pos float3(0, 2, 10.)

float dfSphere( float3 p, float radius )
{
	return length( p )-radius;
}

float dfCapsule( float3 p, float3 a, float3 b, float r )
{
	float3 ab = b-a;
	float3 ap = p-a;

	float t = dot( ab, ap ) / dot( ab, ab );
	t = clamp( t, 0., 1. );

	float3 c = a + t*ab;
	return length( p-c ) - r;
}

float dfPlane( float3 p, float4 n )
{
  // n must be normalized
  return dot(p,n.xyz) + n.w;
}

float dfBox( float3 p, float3 b )
{
  float3 d = abs(p) - b;
  return length(max(d,0.0))
         + min(max(d.x,max(d.y,d.z)),0.0); // remove this line for an only partially signed sdf 
}

float opOnion( float sdf, float thickness )
{
    return abs(sdf)-thickness;
}

float SphereCapsThingy( float3 p )
{
    float d = MAX_DIST;

    float sphereSum = MAX_DIST;
    float w = 5.;
    float h = 5.;
    for( float i=0.; i<w; ++i )
    {
        for( float j=0.; j<h; ++j )
        {
            float inorm = i / w;
            float jnorm = j/h;
        
            float x = inorm * 9. - 4.;
            float y = jnorm * 9. - 4.;
    
            sphereSum = min( sphereSum, dfSphere( p-float3( x, 3, y+10. ), 1.) );
        }
    }

    float capsule2 = dfCapsule( p, float3( -10., 3., 10. ), float3( 10., 3., 10. ), 2. );
    sphereSum = min(sphereSum, capsule2);
    float capsule = max(-sphereSum, dfCapsule( p, float3( -5., 3., 10. ), float3( 5., 3., 10. ), 4) );
    d = max( -sphereSum, capsule );

    return d;
}

float GetDist(float3 p) {    
	float d = MAX_DIST;

    // d = min( d, SphereCapsThingy( p ) );

    /*
    d = min( d, dfBox( p-float3(0, 1, 0), float3( 1, 1, 1 ) ) );
    d = min( d, dfPlane( p - float3(-4., 0., 0.), normalize( float4(1,1,0,sin(iTime)) ) ) );
    float3 sCenter = float3(4., 1., 5.);
    d = min( d, max( -dfBox( p-(sCenter+float3(0,1.5,0)), float3(10,1,10) ),
            opOnion(
                opOnion( 
                    opOnion( 
                        dfSphere( p-sCenter, 2. ), 
                        0.2 ), 
                    .1 ), .05 ) ) );
	c
    d = max( d, -dfSphere( p-CameraPos, 3. ) );
    */

    float3 c = float3(2., 2.+sin(iTime), 2.);
    float3 q = abs(fmod(p,c))-0.5*c;
    d = min( d, dfSphere( q, 0.1 ));
    d = min( d, p.y );

    return d;
}

float RayMarch(float3 ro, float3 rd, float d0) {
	float dO=d0;
    
    for(int i=0; i<MAX_STEPS; i++) {
    	float3 p = ro + rd*dO;
        float dS = GetDist(p);
        dO += dS;
        if(dO>MAX_DIST || dS<SURF_DIST) break;
    }
    
    return dO;
}

float3 GetNormal(float3 p) {
	float d = GetDist(p);
    float2 e = float2(.01, 0);
    
    float3 n = d - float3(
        GetDist(p-e.xyy),
        GetDist(p-e.yxy),
        GetDist(p-e.yyx));
    
    return normalize(n);
}

float GetLight(float3 p, float3 lightPos) {
    // lightPos.xz += float2(sin(iTime), cos(iTime))*2.;
    float3 l = normalize(lightPos-p);
    float3 n = GetNormal(p);
    
    float dif = clamp(dot(n, l), 0., 1.);
    float d = RayMarch(p+n*SURF_DIST*2., l, 0.);
    if(d<length(lightPos-p)) dif *= .1;
    
    return dif;
}


float4 PS( float4 pos : SV_Position ) : SV_Target
{
    float2 uv = (pos.xy - 0.5*iResolution.xy) / (iResolution.x * 0.5);
	uv.y = -uv.y;

	float3 col = float3(0,0,0);
    float3x3 camDir = float3x3( CameraDir._m00, CameraDir._m01, CameraDir._m02, 
                                CameraDir._m10, CameraDir._m11, CameraDir._m12, 
                                CameraDir._m20, CameraDir._m21, CameraDir._m22 );
    
    float3 ro = CameraPos;
    float3 rd = normalize( mul( float3(uv.x, uv.y, 1), camDir ) );

    float d = RayMarch(ro, rd, 0.);
    
    float3 p = ro + rd * d;
    
    float dif = GetLight(p, float3(6, 20, 6.));
    dif += GetLight(p, ro)*.5;
    dif = clamp( dif, 0.1, .9 );
    col = float3(dif, dif, dif);

    // col = rd;
    
	return float4(col,1.0);
}
