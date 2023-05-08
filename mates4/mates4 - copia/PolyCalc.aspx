<%@ Page Language="vb" AutoEventWireup="false" MasterPageFile="~/Site1.Master"
     CodeBehind="PolyCalc.aspx.vb" Inherits="mates4.Polycalc" 
 Title="Calculator -- Polynomials Calculator" %>

<asp:Content ID="Content1" ContentPlaceHolderID="head" runat="server">
    <style type="text/css" media="screen">
        .btnHelp_style {
        text-decoration: none;
        font-weight: bold;
    }
    .btnHelp_style:hover {
        text-decoration: underline;
        color: Red;
    }
    .cuadrBg
    {
    	background-image:Url(/images/cuadr.bmp);
        background-repeat:repeat;
        width:100%;
        border: 2px solid #c0c0c0;
        /* Safari 3-4, iOS 1-3.2, Android 1.6- */
        -webkit-border-radius: 12px; 
        /* Firefox 1-3.6 */
        -moz-border-radius: 12px; 
        /* Opera 10.5, IE 9, Safari 5, Chrome, Firefox 4, iOS 4, Android 2.1+ */
        border-radius: 12px;      
           
    }
     
    .redLn
    {
    	background-image:Url(/images/cuadr2.jpg);
        background-repeat:repeat-y;
        -moz-border-radius: 15px;
        border-radius: 15px;
    }
    .lbtn {width:35px;}
    .keyboardPolyC {margin-left: 18px;}
    .quickStart
    {
         margin-bottom:5px;
    }
    .save { font-family:Courier New, Courier, monospace; font-size: medium; vertical-align:middle;
    }
    </style>
</asp:Content>
<asp:Content ID="Content2" ContentPlaceHolderID="content2L" runat="server">
        <h3 class="TableTot">
        <asp:Label ID="lblTitulo" runat="server" CssClass="keyboardPolyC">Polynomials &amp; Scientific Calculator</asp:Label>
        </h3>

        <asp:Label CssClass="keyboardMargin" ID="lblUpdated" runat="server" Text="Label" Font-Names="Arial" Font-Size="Small">
        </asp:Label>

</asp:Content>
<asp:Content ID="Content3" ContentPlaceHolderID="maincontent" runat="server">

            <asp:Table ID="Table1" runat="server" Width="700px" HorizontalAlign="center" CssClass="TableTot">
            
                <asp:TableRow>
                    <asp:TableCell>

                        <asp:Table CssClass="save" ID="Table2" runat="server" Width="100%">
                        
                            <asp:TableRow>
                                <asp:TableCell VerticalAlign="Top" HorizontalAlign ="center">
                                    <asp:Literal  ID="litComment1" runat="server">
                                    Welcome to Free Online Polynomials, and Scientific, calculator.
                                    You can perform operations with polynomials (+, -, *, /), find the roots, 
                                    logarithmic, trigonometric and exponential functions, or define custom functions,
                                    in the field of real or complex numbers. It also incorporates constants such as 'pi' and 'e'.
                                    Also you can enter 
                                    </asp:Literal>
                                </asp:TableCell>
                                <asp:TableCell HorizontalAlign ="center">
                                    <asp:Literal  ID="litComment2" runat="server">
                                    binary, octal or hexadecimal numbers by prefixing &b, &o and &h
                                    and get the results in each of these bases by checking the appropriate box.
                                    Other options offered are the calculation of derivatives, integrals
                                    or Lagrange interpolation, and matrix calculation and resolution
                                    systems of equations.
                                    In many cases, you may consult the examples or the intermediate steps.
                                    </asp:Literal>
                                </asp:TableCell>
                            </asp:TableRow>
                            <asp:TableRow >
                                <asp:TableCell VerticalAlign="Middle" ColumnSpan="2" CssClass="save" Width="50%" >
                                    &nbsp;&nbsp;&nbsp;&nbsp;
                                    Save:<br />
                                    &nbsp;&nbsp;&nbsp;&nbsp;
                                    <asp:ImageButton ID="btnSave" ImageAlign="AbsMiddle" ToolTip="Save current session into a coded text file" runat="server" ImageUrl="~/images/Save.png" />
                                    &nbsp;
                                    <asp:FileUpload id="FileUpload1"
                                        runat="server">
                                    </asp:FileUpload>
                                    &nbsp;
                                    <asp:Button  ID="btnUpload1" 
                                          runat ="server" Text="Upload session coded file" />
                                    &nbsp;
                                </asp:TableCell>
                            </asp:TableRow>
                            <asp:TableRow>
                                <asp:TableCell HorizontalAlign="center" VerticalAlign="Middle" CssClass="keyboardPolyC" ColumnSpan="2">
                                    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;
                                    <asp:DropDownList  ID="DropHist" runat="server" CausesValidation="false" EnableViewState="true" 
                                        Width="80%" AutoPostBack="True" BackColor="#F0F0F0" ToolTip="Queries' history">
                                    </asp:DropDownList>
                                    <asp:Button ID="btnClearDropQuery" runat="server" Text="Record Clear" />
                                </asp:TableCell>
                            </asp:TableRow>
                            <asp:TableRow>
                                <asp:TableCell HorizontalAlign="center">
                                    <asp:DropDownList ID="cbFns" runat="server" Width="140px" AutoPostBack="True">
                                    </asp:DropDownList>
                                </asp:TableCell>
                                <asp:TableCell HorizontalAlign="center">
                                    <asp:DropDownList ID="cbExamples" runat="server" Width="140px" AutoPostBack="True">

                                    <asp:ListItem Text="Select">Examples:</asp:ListItem>
                                    <asp:ListItem>---Basic operations:---</asp:ListItem>
                                    <asp:ListItem>2×3+4</asp:ListItem>
                                    <asp:ListItem>2^3^2</asp:ListItem>
                                    <asp:ListItem>√(2+2)</asp:ListItem>
                                    <asp:ListItem>1600/((1-(1+1/4)^30)/(1-(1+1/4)))</asp:ListItem>
                                    <asp:ListItem>m!/(n!*(m-n)!)@m=5@n=3</asp:ListItem>
                                    <asp:ListItem>gcd(12;36;6;24) ' Greatest common divisor</asp:ListItem>
                                    <asp:ListItem>lcm(12;18;60) ' Least common multiple</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Custom functions:---</asp:ListItem>
                                    <asp:ListItem>P(x)/Q(x)?P(x)=(x+1)*(x-1)|Q(x)=x+1</asp:ListItem>
                                    <asp:ListItem>y^2?y=(x+i)*(x-i)</asp:ListItem>
                                    <asp:ListItem>f(3,1)?f(x,y)=y-x</asp:ListItem>
                                    <asp:ListItem>f(2,-2)?f(x,y)=y-x,x+y|x*y,-2*x*y</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Evaluating:---</asp:ListItem>
                                    <asp:ListItem>x^2-9*x+20@x=5</asp:ListItem>
                                    <asp:ListItem>x^2-9x+20@x=4.5</asp:ListItem>
                                    <asp:ListItem>2+x/(y*z)@x=12|y=4|z=-3</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Hexadecimal, octal, binary, decimal:</asp:ListItem>
                                    <asp:ListItem>&amp;hF+&amp;o17+&amp;b1111+&amp;d15</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Logical operators:</asp:ListItem>
                                    <asp:ListItem>(&amp;hff OR &amp;h0f) AND &b111</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Modulus:</asp:ListItem>
                                    <asp:ListItem>((x5+3x3+4)*(6x6+4x3))%11</asp:ListItem>
                                    <asp:ListItem>(x7+x6+x5+x4+x3+x2)mod(x5+x)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---POLYNOMIALS:</asp:ListItem>
                                    <asp:ListItem>---Add, subs., multiply, divide:</asp:ListItem>
                                    <asp:ListItem>(x-1)+(x+1)</asp:ListItem>
                                    <asp:ListItem>(x-1)-(x+1)</asp:ListItem>
                                    <asp:ListItem>(x-1)*(x+1)</asp:ListItem>
                                    <asp:ListItem>(x^2-1)/(x+1)</asp:ListItem>
                                    <asp:ListItem>gcd((3x2-3)(x+2),-(3x-3)(x+2),(2x+4))// Greatest common divisor</asp:ListItem>
                                    <asp:ListItem>poly(-1,2) // find a polynomial given the roots</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Root finder:</asp:ListItem>
                                    <asp:ListItem>roots(x16-1)</asp:ListItem>
                                    <asp:ListItem>factor(x16-1)</asp:ListItem>
                                    <asp:ListItem>(x-1)*(x+1)=0</asp:ListItem>
                                    <asp:ListItem>(5*x+2)*(3*x-1)=0</asp:ListItem>
                                    <asp:ListItem>x^2-9x+20=0</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Partial fraction descomposition:</asp:ListItem>
                                    <asp:ListItem>residue((-4s+8)/(s2+6s+8))</asp:ListItem>
                                    <asp:ListItem>residue((s-2)/((s-1)(s+2)(s-3)))</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Orthogonality:</asp:ListItem>
                                    <asp:ListItem>orthog(-1,1,2*t,3*t^2+5)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Interpolation:---</asp:ListItem>
                                    <asp:ListItem>lagrangianinterpolation(-2,1|0,-1|2,1)</asp:ListItem>
                                    <asp:ListItem>lagrangianinterpolation(-1,1,10,abs(x))// 10 intervals at [-1,1] with Chebyshev nodes for f(x)=abs(x)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Derivatives:---</asp:ListItem>
                                    <asp:ListItem>Dx(cosh(x))</asp:ListItem>
                                    <asp:ListItem>Dy(sin(x^2*y^2+y))</asp:ListItem>
                                    <asp:ListItem>Dy(z)?z=x+y|x=y^2</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---2nd.derivative:---</asp:ListItem>
                                    <asp:ListItem>D2x(3x^2-2x+5)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Jacobian:---</asp:ListItem>
                                    <asp:ListItem>jacobian(1,000x^2-2x+5,sin(x2)|xsinh(y),xasinh(y))</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Vector operation:</asp:ListItem>
                                    <asp:ListItem>dot(3,-3,1|4,9,2)</asp:ListItem>
                                    <asp:ListItem>cross(3,-3,1|4,9,2)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---LIMITS:</asp:ListItem>
                                    <asp:ListItem>lim((x2-16)/(x-4),x,4)</asp:ListItem>
                                    <asp:ListItem>lim(sin(x)/x,x,0)</asp:ListItem>
                                    <asp:ListItem>lim(ln(x)/x,x,0)</asp:ListItem>
                                    <asp:ListItem>lim((5t4-4t2-1)/(10-t-9t3),t,1)</asp:ListItem>
                                    <asp:ListItem>lim(x*e^x,x,-infinity)</asp:ListItem>
                                    <asp:ListItem>lim(exp(x)/x^2,x,∞)</asp:ListItem>
                                    <asp:ListItem>lim(x*ln(x),x,0)</asp:ListItem>
                                    <asp:ListItem>lim(x^(1/x),x,infinity)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---MATRIX operations:</asp:ListItem>
                                    <asp:ListItem>(1,0|0,-1|^|-1)|*|1,0|0,-1</asp:ListItem>
                                    <asp:ListItem>(1,2|3,4|+|1,0|0,1)|*|-1,0|0,1</asp:ListItem>
                                    <asp:ListItem>((1,0|0,3|-|5,0|3,-2)|^|-1)|*|(-4,0|-3,5)</asp:ListItem>
                                    <asp:ListItem>(x,0|0,1|^|-1)|*|a?a=x,0|0,1</asp:ListItem>
                                    <asp:ListItem>x+1,0|0,-1|^|-1|*|a?a=(x+1),0|0,-1</asp:ListItem>
                                    <asp:ListItem>A+(-B*C)?A=1,2|3,-1||B=A|C=1,0|0,1</asp:ListItem>
                                    <asp:ListItem>Dx(x^2-sin(x),-6*x^2-x+3|tan(x)+x^2,sinh(x))</asp:ListItem>
                                    <asp:ListItem>integral(A)dy @ A = x^2*y-y^2;-6*x*y^2-x+3|1/(y+1);sinh(y)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Inverse matrix:</asp:ListItem>
                                    <asp:ListItem>A^-1?A=1,2|3,-1</asp:ListItem>
                                    <asp:ListItem>A^-1*A?A=1,2|3,-1</asp:ListItem>
                                    <asp:ListItem>A^-1*A?A=x,y|2,-3</asp:ListItem>
                                    <asp:ListItem>a*a^-1|*|a|-|a?a=cos(x),-sin(x)|sin(x),cos(x)</asp:ListItem>
                                    <asp:ListItem>A*A^-1?A=(3,a,a,a,a|a,3,a,a,a|a,a,3,a,a|a,a,a,3,a|a,a,a,a,3)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Determinant:</asp:ListItem>
                                    <asp:ListItem>det(A)?A=(1-x;2|3;2-x)</asp:ListItem>
                                    <asp:ListItem>roots(det(A))?A=(1-x;2|3;2-x)</asp:ListItem>
                                    <asp:ListItem>det(A)=0?A=(1-x;2|3;2-x)</asp:ListItem>
                                    <asp:ListItem>roots(det(A))@A=(3,a,a,a,a|a,3,a,a,a|a,a,3,a,a|a,a,a,3,a|a,a,a,a,3)</asp:ListItem>
                                    <asp:ListItem>det(A)|-|(4a+3)*(3-a)^4?A=(3,a,a,a,a|a,3,a,a,a|a,a,3,a,a|a,a,a,3,a|a,a,a,a,3)</asp:ListItem>
                                    <asp:ListItem>det(A-B)=0?A=(1,2|3,2)|B=λ*(1,0|0,1)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Identity matrix:</asp:ListItem>
                                    <asp:ListItem>Identity(3) // 3x3 Identity matrix</asp:ListItem>
                                    <asp:ListItem>Identity(3,4) // 3x4 Identity matrix</asp:ListItem>
                                    <asp:ListItem>---Trace:</asp:ListItem>
                                    <asp:ListItem>trace(1-x;2|3;2-x)</asp:ListItem>
                                    <asp:ListItem>---Transpose:</asp:ListItem>
                                    <asp:ListItem>transpose(1-x;2|3;2-x)</asp:ListItem>
                                    <asp:ListItem>---Cofactor:</asp:ListItem>
                                    <asp:ListItem>cof(1;3;1|1;1;2|2;3;4)</asp:ListItem>
                                    <asp:ListItem>---Adjoint:</asp:ListItem>
                                    <asp:ListItem>Adj(1;3;1|1;1;2|2;3;4)</asp:ListItem>
                                    <asp:ListItem>---Echelon:</asp:ListItem>
                                    <asp:ListItem>Echelon(1,2,1|-2,-3,1|3,5,0) /* see http://en.wikipedia.org/wiki/Row_echelon_form */</asp:ListItem>
                                    <asp:ListItem>---Rank:</asp:ListItem>
                                    <asp:ListItem>Rank(1,2,1|-2,-3,1|3,5,0) /* see http://en.wikipedia.org/wiki/Rank_(linear_algebra) */</asp:ListItem>
                                    <asp:ListItem>---EigenValues:</asp:ListItem>
                                    <asp:ListItem>eigenvalues(2;1|1;2)</asp:ListItem>
                                    <asp:ListItem>---Eigenvectors:</asp:ListItem>
                                    <asp:ListItem>eigenvectors(2;1|1;2)</asp:ListItem>
                                    <asp:ListItem>---Jordan form:</asp:ListItem>
                                    <asp:ListItem>A*T-T*jordan(A)?A=2,0,1|0,2,0|1,0,2|T=eigenvectors(A)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---SYSTEM OF EQUATIONS:---</asp:ListItem>
                                    <asp:ListItem>---Linear:---</asp:ListItem>
                                    <asp:ListItem>Xc=1/(2*pi*f*C)?Xc=50|f=10e6</asp:ListItem>
                                    <asp:ListItem>Xc=1/(2*pi*f*C)?Xc=50&#937;|f=10e6Hz|Hz=1|&#937;=1|pF=1e-12F|F=1</asp:ListItem>
                                    <asp:ListItem>x+y=2|x-y=1</asp:ListItem>
                                    <asp:ListItem>---Non-Linear:---</asp:ListItem>
                                    <asp:ListItem>x2-10x+y2+8=0|x*y2+x-10y+8=0</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---INTEGRATION:</asp:ListItem>
                                    <asp:ListItem>---Integration of polynomials:</asp:ListItem>
                                    <asp:ListItem>integral(6x-2)</asp:ListItem>
                                    <asp:ListItem>integral(3x2+y)dx</asp:ListItem>
                                    <asp:ListItem>&#8747;(x2+y)dy</asp:ListItem>
                                    <asp:ListItem>&#8747;(√x+(1/√x))^2|&#8747;((√x+(1/√x))^2)dx|(√x+(1/√x))^2</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Inmediate integrals:</asp:ListItem>
                                    <asp:ListItem>&#8747;(sin(x))dx</asp:ListItem>
                                    <asp:ListItem>&#8747;(ln(x))dx</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Trigonometric integrals:</asp:ListItem>
                                    <asp:ListItem>&#8747;(sin2(x)*cos(x))dx</asp:ListItem>
                                    <asp:ListItem>&#8747;(tan2(x))dx</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Definite integrals:</asp:ListItem>
                                    <asp:ListItem>integral(1,2,x)dx</asp:ListItem>
                                    <asp:ListItem>integral(1,2,x*y)dy</asp:ListItem>
                                    <asp:ListItem>&#8747;(1,2,x)dx</asp:ListItem>
                                    <asp:ListItem>&#8747;(a,b,x*y)dx?a=1|b=2</asp:ListItem>
                                    <asp:ListItem>&#8747;(a,b,S(x))dx,pi(10^(3/2)-1)/27?a=0|b=1|f(x)=x3|S(x)=2*pi*f(x)*sqr(1+(Dx(f(x)))^2)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Partial fractions integration:</asp:ListItem>
                                    <asp:ListItem>&#8747;[1/(x(x-2)^2)]dx</asp:ListItem>
                                    <asp:ListItem>(x(x-2)^2)*Dx(&#8747;1/(x(x-2)^2)dx)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Trigonometric integration:</asp:ListItem>
                                    <asp:ListItem>&#8747;(sec(x)*tan2(x))dx</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Integration by parts:</asp:ListItem>
                                    <asp:ListItem>13*&#8747;(sin(3*x)*exp(2*x))dx</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Improper integrals</asp:ListItem>
                                    <asp:ListItem>lim(integral(0;b;1/(1+x2))dx,b,infinity)</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---DIFFERENTIAL EQUATIONS:</asp:ListItem>
                                    <asp:ListItem>---First order linear:</asp:ListItem>
                                    <asp:ListItem>Dt(y)=9.8-0.196y@y(0)=48</asp:ListItem>
                                    <asp:ListItem>t*Dt(y)+2*y=t^2-t+1@y(1)=0.5</asp:ListItem>
                                    <asp:ListItem>2*Dt(y)-y=4*sin(3t)@y(0)=_y0</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---First order separable:</asp:ListItem>
                                    <asp:ListItem>Dx(y)=6y^2x@y(1)=1/25</asp:ListItem>
                                    <asp:ListItem>Dx(y)=(3x2+4x-4)/(2y-4)@y(1)=3</asp:ListItem>
                                    <asp:ListItem>Dx(y)=exp(-y)*(2x-4)@y(5)=0</asp:ListItem>
                                    <asp:ListItem>1/(exp(-y)*(2x-4))*Dx(y)=1@y(5)=0</asp:ListItem>
                                    <asp:ListItem>Dθ(r)=r^2/θ@r(1)=2</asp:ListItem>
                                    <asp:ListItem>Dt(y)=exp(y-t)*sec(y)*(1+t^2)@y(0)=0</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---First order exact:</asp:ListItem>
                                    <asp:ListItem>2xy-9x2+(2y+x2+1)*Dx(y)=0@y(0)=-3</asp:ListItem>
                                    <asp:ListItem>2xy2+4=2*(3-x2y)*Dx(y)@y(-1)=8</asp:ListItem>
                                    <asp:ListItem>(2xy2+4)/(2*(3-x2y)*Dx(y))=1@ y(-1)=8</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---First order Bernoulli:</asp:ListItem>
                                    <asp:ListItem>Dx(y)+(4/x)*y = x3*y2@y(2)=-1</asp:ListItem>
                                    <asp:ListItem>Dx(y)=5y+exp(-2x)*y^-2@y(0)=2</asp:ListItem>
                                    <asp:ListItem>6*Dx(y)-2y = xy4 @ y(0)=-2</asp:ListItem>
                                    <asp:ListItem>Dx(y)+y/x-sqr(y)=0 @ y(1)=0</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---First order substitution:</asp:ListItem>
                                    <asp:ListItem>xyDx(y)+4x2+y2=0 @ y(2)=-7</asp:ListItem>
                                    <asp:ListItem>Dx(y)=exp(9y-x) @ y(0)=0</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---Euler's method:</asp:ListItem>
                                    <asp:ListItem>Eulers(_step;N;Dt(y)-y=-1/2*exp(t/2)*sin(5t)+5*exp(t/2)*cos(5t))?_step=0.1|N=10|y(0)=0</asp:ListItem>
                                    <asp:ListItem>                </asp:ListItem>
                                    <asp:ListItem>---MISCELLANEOUS:---</asp:ListItem>
                                    <asp:ListItem>isolate(C,w*L=1/(w*C))</asp:ListItem>
                                    <asp:ListItem>isolate(x,x-y+3=-1)</asp:ListItem>
                                    </asp:DropDownList>
                                </asp:TableCell>
                            </asp:TableRow> 

                            <asp:TableRow>
                                <asp:TableCell Width="50%" HorizontalAlign="Center">
                                    <asp:Table Width="90%" runat="server" ID="Table10">
                                    
                                        <asp:TableRow >
                                        <asp:TableCell HorizontalAlign="center">
                                            <asp:Label ID="lblExpression" runat="server" Text="Expression to operate:"></asp:Label>
                                        </asp:TableCell>
                                        </asp:TableRow>
                                    </asp:Table>
                                </asp:TableCell> 
                                <asp:TableCell Width="50%" HorizontalAlign="Center">
                                    <asp:Table ID="Table20" Width="90%" runat="server">
                                        <asp:TableRow >
                                        <asp:TableCell HorizontalAlign="center">
                                            <asp:Label ID="lblVariables" runat="server" Text="Variable(s) (if any): "></asp:Label>
                                        </asp:TableCell>
                                        </asp:TableRow> 
                                    </asp:Table> 
                                </asp:TableCell> 
                            </asp:TableRow> 
                            <asp:TableRow>
                                <asp:TableCell HorizontalAlign="center" VerticalAlign="Middle" Width="50%">

                                    <asp:TextBox spellcheck="false" ID="tbQuery" CssClass="keyboardPolyC" runat="server" TextMode="MultiLine" Rows="8" Width="95%" Font-Names="Calibri, Arial"></asp:TextBox>

                                </asp:TableCell>

                                <asp:TableCell HorizontalAlign="center" VerticalAlign="Middle" Width="50%" CssClass="keyboardPolyC">

                                    <asp:TextBox spellcheck="false" ID="tbVarsWithName" runat="server" TextMode="MultiLine" Rows="8" Width="95%" Font-Names="Calibri, Arial"></asp:TextBox>

                                </asp:TableCell>
                            </asp:TableRow>
                            <asp:TableRow>
                                <asp:TableCell HorizontalAlign="Left" VerticalAlign="Top" CssClass="keyboardPolyC" Width="45%">
                                    <asp:Table ID="Table4" runat="server">
                                    
                                    <asp:TableRow>
                                    <asp:TableCell RowSpan="5" HorizontalAlign="left" VerticalAlign="Top">
                                        <asp:Table ID="Table5" runat="server" CssClass="keyboardPolyC" Width="50%">
                                        
                                            <asp:TableRow>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnIntegral" runat="server" Text="&#8747;" CssClass="lbtn" ToolTip="Integral symbol" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnLP" runat="server" Text="(" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnRP" runat="server" Text=")" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnOneHalf" runat="server" Text="½" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnPi" runat="server" Text="П" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnClearQuery" runat="server" Text="Clear Query" Width="95" />
                                                </asp:TableCell>
                                            </asp:TableRow>
                                            <asp:TableRow>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnx" runat="server" Text="x" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnPow" runat="server" Text="^" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnDiv" runat="server" Text="/" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnMult" runat="server" Text="*" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnMinus" runat="server" Text="-" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnClearVars" runat="server" Text="Clear Defs." Width="95"  />
                                                </asp:TableCell>
                                            </asp:TableRow>
                                            <asp:TableRow>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnSqrt" runat="server" Text="√" CssClass="lbtn" ToolTip="Square root symbol" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn7" runat="server" Text="7" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn8" runat="server" Text="8" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn9" runat="server" Text="9" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnPlus" runat="server" Text="+" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnClearBoth" runat="server" Text="Clear All" Width="95" />
                                                </asp:TableCell>
                                            </asp:TableRow>
                                            <asp:TableRow>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnHexa" runat="server" Text="&h" CssClass="lbtn" ToolTip="Hexadecimal prefix" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn4" runat="server" Text="4" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn5" runat="server" Text="5" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn6" runat="server" Text="6" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnEqual" runat="server" Text="=" CssClass="lbtn" />
                                                </asp:TableCell>
                                                
                                                <asp:TableCell RowSpan="3" HorizontalAlign="center" VerticalAlign="top">
                                                   &nbsp;
                                                </asp:TableCell>
                                            </asp:TableRow>
                                            <asp:TableRow>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnOctal" runat="server" Text="&o" CssClass="lbtn" ToolTip="Octal prefix" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn1" runat="server" Text="1" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn2" runat="server" Text="2" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn3" runat="server" Text="3" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnEqualZero" runat="server" Text="=0" CssClass="lbtn" />
                                                </asp:TableCell>
                                            </asp:TableRow>
                                            <asp:TableRow>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnBin" runat="server" Text="&b" CssClass="lbtn" ToolTip="Binary prefix" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btn0" runat="server" Text="0" CssClass="lbtn"  />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnDot" runat="server" Text="." CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell >
                                                    <asp:Button ID="btnExp" runat="server" Text="e" CssClass="lbtn" />
                                                </asp:TableCell>
                                                <asp:TableCell>
                                                    <asp:Button ID="btnCE" runat="server" Text="CE" CssClass="lbtn" ToolTip="Clear Entry. Clears last character." />
                                                </asp:TableCell>
                                            </asp:TableRow>

                                            <asp:TableRow>
                                                <asp:TableCell ColumnSpan="5" VerticalAlign="Bottom" HorizontalAlign="Center" Width="100%">
                                                    <asp:Button ID="btnCalc" runat="server" Text="Calculate (F8 key)" style="height: 42px; width:100%;" Font-Bold="True" />
                                                </asp:TableCell>
                                            </asp:TableRow>
                                        </asp:Table>
                                    </asp:TableCell>
                                    </asp:TableRow>
                                    </asp:Table>
                                </asp:TableCell>
                                <asp:TableCell Width="45%" HorizontalAlign="Left" VerticalAlign="Top">


                                    <asp:Table GridLines="Both" ID="Table3" runat="server" Width="100%" HorizontalAlign="left">
                                    
                                        <asp:TableRow>
                                        <asp:TableCell>
                                            <asp:CheckBox ID="chkCaseSensitive" runat="server" Text="Case sensitive" Checked="True" ToolTip="Distinguish between lower and uppercase letters" />
                                        </asp:TableCell>
                                        <asp:TableCell>
                                            <asp:CheckBox ID="chkRound" runat="server" Text="Round" Checked="True" ToolTip="Round output up to 3 decimals" />
                                        </asp:TableCell>
                                        <asp:TableCell>
                                            <asp:RadioButton ID="rbI" runat="server" Text="i" Checked="True" AutoPostBack="True" ToolTip='Employ "i" as imaginary symbol' />&nbsp;
                                            <asp:RadioButton ID="rbJ" runat="server" Text="j" AutoPostBack="True" ToolTip='Employ "j" as imaginary symbol' />
                                        </asp:TableCell>
                                        </asp:TableRow>

                                        <asp:TableRow HorizontalAlign="Left">
                                        <asp:TableCell >
                                            <asp:Checkbox ID="chkIgnoreSpaces" runat="server" Text="Ignore Spaces" Checked="False" ToolTip="Ignore Spaces" />
                                        </asp:TableCell>
                                        <asp:TableCell>
                                            <asp:CheckBox ID="chkENG" runat="server" Text="Eng." Checked="True" ToolTip="Output, eventually, power multiples of 3 (1.2e3, 12e6, ..)" />
                                        </asp:TableCell>
                                        <asp:TableCell>
                                            <asp:CheckBox ID="chkBinary" runat="server" Text="Binary" ToolTip="Output the result in binary base" />
                                        </asp:TableCell>
                                        </asp:TableRow>

                                        <asp:TableRow>
                                        <asp:TableCell HorizontalAlign="Left">
                                            <asp:CheckBox ID="chkDetail" runat="server" Text="Detail" ToolTip="Output, if available, detailed info" />
                                        </asp:TableCell> 
                                        <asp:TableCell>
                                            <asp:CheckBox ID="chkFractions" runat="server" Text="Fractions" Checked="true" ToolTip="Output, when possible, the result using fractions" />
                                        </asp:TableCell>
                                        <asp:TableCell>
                                            <asp:CheckBox ID="chkOctal" runat="server" Text="Octal" ToolTip="Output the result in octal base" />
                                        </asp:TableCell>
                                        </asp:TableRow>

                                        <asp:TableRow>
                                        <asp:TableCell >
                                        </asp:TableCell>
                                        <asp:TableCell>
                                        </asp:TableCell>
                                        <asp:TableCell>
                                            <asp:CheckBox ID="chkHexa" runat="server" Text="Hexadecimal" ToolTip="Output the result in hexadecimal base" />
                                        </asp:TableCell>
                                        </asp:TableRow>
                                        <asp:TableRow>
                                            <asp:TableCell ColumnSpan="3">
                                                <asp:Table runat="server">
                                                <asp:TableRow>
                                                <asp:TableCell HorizontalAlign="Left">
                                                    <asp:CheckBox ID="chkVar1Char" Checked="true" runat="server" Text="Vars.name 1char" CausesValidation="True" ToolTip="The name of a variable will be 1 character (checked) or max. 2 chars.(unchecked)" />
                                                    <br />
                                                    <asp:Literal ID="Literal1" runat="server">&nbsp;&nbsp;&nbsp;</asp:Literal>
                                                    <asp:CheckBox ID="chkNumInVar" runat="server" Text="Allow trailing numbers in variables name" CausesValidation="True" ToolTip="If a vars. name length is 2 chars., the second character may be a number" />
                                                </asp:TableCell>
                                                </asp:TableRow>
                                                </asp:Table>
                                            </asp:TableCell>
                                        </asp:TableRow>
                                        <asp:TableRow>
                                        <asp:TableCell ColumnSpan="3" HorizontalAlign="Left" >
                                            <asp:Table ID="Table6" runat="server" GridLines="Both">
                                            
                                                <asp:TableRow>
                                                    <asp:TableCell ColumnSpan="2">
                                                        <asp:LinkButton ID="btnHelp" CssClass="btnHelp_style" runat="server"><span style="font-size:x-small;">User's guide (.pdf)v8.3.110</span></asp:LinkButton>
                                                        <asp:PlaceHolder ID="PlaceHolder1" runat="server"><a id="A1" href="quickStart.aspx" class="quickStart" target="_blank" runat="server">
                                                            <br />
                                                            Quick Start</a><br />
                                                            <asp:LinkButton ID="linkHelp2" CssClass="quickStart" runat="server">Html version</asp:LinkButton>
                                                        </asp:PlaceHolder>
                                                        <br />
                                                        <asp:LinkButton ID="btnHelp4" CssClass="quickStart" runat="server">Manual de usuario (.pdf)</asp:LinkButton>

                                                    </asp:TableCell>
                                                </asp:TableRow>
                                            </asp:Table>
                                        </asp:TableCell>
                                        </asp:TableRow>

                                     </asp:Table> 


                                </asp:TableCell>
                            </asp:TableRow>
                        </asp:Table> 
                    </asp:TableCell>
                </asp:TableRow>

            </asp:Table>
            <asp:Table ID="tblOutput" runat="server" HorizontalAlign="center" Width="90%" Height="100%">
            
            <asp:TableRow Height="100%">
            <asp:TableCell Height="100%" VerticalAlign="Top">
                <div style="margin-left:5px; font-size:medium; font-family:Courier New, Courier, monospace;">
                    <asp:Label CssClass="save" ID="lblResult" runat="server" Text="Result:"></asp:Label><br />
                    <asp:CustomValidator ID="tbQueryValidator" runat="server" ErrorMessage="CustomValidator"></asp:CustomValidator>
                    <br />
                <asp:Literal ID="litResponse" runat="server"></asp:Literal>
                <asp:HiddenField ID="num" Value="0" runat="server" />
                <asp:HiddenField ID="histQ" Value="" runat="server" />
                <asp:HiddenField ID="histV" Value="" runat="server" />
                <asp:HiddenField ID="options" Value="" runat="server" />
    </div>

    </asp:TableCell>
    </asp:TableRow>
    </asp:Table>



</asp:Content>
