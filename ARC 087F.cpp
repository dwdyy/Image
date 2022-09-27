#include<bits/stdc++.h>
using namespace std;
#define int long long
typedef long long ll;
const int N = 3e5+7;
const int maxn = 3e5+7;
const int mod = 998244353;
#define pb push_back
 

 
namespace poly{
    const ll g= 3;
    ll a[maxn],b[maxn];
    int r[maxn];
    inline ll open(int x){
        ll lim=1,k=0;
        while(2*x>=lim) lim<<=1,k++;
        for(int i=0;i<lim;i++) r[i] = r[i>>1]>>1 | ((i&1)<<k-1);
        return lim;
    }
    ll qpow(ll a,ll b=mod-2)
    {
        ll ans=1;
        for(;b;b>>=1,a=a*a%mod) if(b&1) ans = ans * a %mod;
        return ans;
    }
 
    const ll invg = qpow(g);
 
    void ntt(ll *f,ll lim,int opt)
    {
    for(int i=0;i<lim;i++) if(i < r[i]) swap(f[i],f[r[i]]);
    for(int k=2;k<=lim;k<<=1)
        {
            ll len = k>>1,tg=qpow(opt==1 ? g : invg,(mod-1)/k);
            for(int l=0;l<lim;l+=k)
            {
                ll buf=1;
                for(int now=l;now<len+l;now++)
            {
                ll dwd = f[now+len] * buf%mod;
                f[now+len] = (f[now] -dwd + mod )%mod;
                f[now] = (f[now]+dwd)%mod;
                buf = buf * tg%mod;
            }
        }
    }
    if(opt!=1)
    for(ll i=0,dwd=qpow(lim);i<lim;i++)
    f[i] = f[i] * dwd %mod;
}
 
     
}
int n;
int G,cnt;
int siz[N];
ll ans,farc[N],inv[N],f[N];
ll mxrt;
vector<int>gra[N];
void dfs(int x,int fa)
{
    int mx=0;
    siz[x]=1;
    for(auto v:gra[x]){
        if(v==fa) continue;
        dfs(v,x);
        siz[x]+=siz[v];
        mx = max(mx,siz[v]);
    }
     
    mx = max(mx,n-siz[x]);
    if(mx <mxrt){
        mxrt=mx;
        cnt=1;
        G=x;
    }
    else if(mx==mxrt){
         
        cnt++;
        G=x;
    }
}
ll qpow(ll x,ll b)
{
    ll ans = 1;
    for(;b;b>>=1,x=x*x%mod) if(b&1) ans = ans * x %mod;return ans;
}
ll cnm(int x,int y){
    if(y > x) return 0;
    return farc[x] * inv[y]%mod * inv[x-y] %mod;
}
 
struct node{
    vector<ll>dwd;
    int size;
    friend bool operator < (node x,node y)
    {
        return x.size > y.size;
    }  
};
priority_queue<node>wdnmd;
signed main()
{
    mxrt=1e9;
    cin >> n;
    farc[0]=1;
    for(int i=1;i<=100000;i++) farc[i] = farc[i-1] * i %mod;
    inv[100000]=qpow(farc[100000],mod-2);
    for(int i=100000-1;i>=1;i--) inv[i] = inv[i+1] * (i+1) %mod;
    inv[0]=1;
     
    for(int i=1;i<n;i++){
       int u,v; cin >> u >> v;
       gra[u].pb(v);gra[v].pb(u); 
    }
     
    dfs(1,0);dfs(G,0);
    if(cnt>=3){
        cout << farc[n/2] * farc[n/2] %mod;
        exit(0);
    }
    for(auto v : gra[G])
    {   
        int x = siz[v];
        node icu;
        icu.dwd.pb(1);
        icu.size=x;
        for(int k=1;k<=x;k++) {
            ll tmd=cnm(x,k)*cnm(x,k)%mod*farc[k]%mod;
            icu.dwd.pb(tmd);
        }
        wdnmd.push(icu);
    }
    while(wdnmd.size()!=1)
    {
        node a1 = wdnmd.top();wdnmd.pop();
        node a2 = wdnmd.top();wdnmd.pop();
        node a3; a3.size=a1.size+a2.size+1;
        ll lim = poly::open(max(a1.size,a2.size));
         
        for(int i=0;i<=lim;i++){
            poly::a[i]=0;
            if(i<=a1.size) poly::a[i] = a1.dwd[i];
            poly::b[i]=0;
            if(i<=a2.size) poly::b[i] = a2.dwd[i];
        }
        poly::ntt(poly::a,lim,1);
        poly::ntt(poly::b,lim,1);
        for(int i=0;i<lim;i++) poly::b[i] = poly::b[i] * poly::a[i]%mod;
        poly::ntt(poly::b,lim,0);
         
        for(int i=0;i<=a3.size;i++) a3.dwd.pb(poly::b[i]);
        wdnmd.push(a3);
    }
    node bbb = wdnmd.top();
    for(int i=0;i<=n;i++) f[i] = bbb.dwd[i];
     
    for(int i=0;i<=n;i++)
    {
        ans = (ans + (i&1 ? -1ll : 1ll) * f[i] * farc[n-i]%mod)%mod;
    }
    cout << (ans%mod+mod)%mod;
    return 0;
}