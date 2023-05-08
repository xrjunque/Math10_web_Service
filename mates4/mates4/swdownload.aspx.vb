Imports System.Data
Imports System.IO

Partial Public Class FreeSoftware
    Inherits System.Web.UI.Page

    Dim lengua As String
    Private dt As New DataTable
    Dim nID As Int32
    Private Sub Page_PreRender(sender As Object, e As EventArgs) Handles Me.PreRender
        'Introducir aquí el código de usuario para inicializar la página
        'nID = C_SessionWeb.LCID Mod 10000
        Try
            C_sessionWeb.contador(Page, Request, Response)
        Catch ex As Exception

        End Try
        nID = C_sessionWeb.getID(Page)
        'If Not Page.IsPostBack Then
        '    ClientScript.RegisterStartupScript(
        '        Me.GetType(), "myScript",
        '       "<script language=javascript>" + vbCrLf +
        '       "function img3() { this.src='/images/download3.bmp'; }" + vbCrLf +
        '       "</script>" + vbCrLf)

        'End If
        'If C_sessionWeb.BrowserIsMobile Then

        '    Dim ctl As WebControls.Table = Me.Master.FindControl("Tabletot")
        '    If ctl IsNot Nothing Then
        '        ctl.Width = Request.Browser.ScreenPixelsWidth
        '    End If
        'End If
        Try
            HttpRuntime.Close()
        Catch ex As Exception

        End Try
        'lengua = C_SessionWeb.idioma(nID)
        If True Then ' <--- cambiar a FALSE para mensaje "FUERA DE SERVICIO TEMPORALMENTE"
            inicio()
        Else
            msg.Text = "Temporarily, out of service."
        End If
    End Sub
    Sub inicio()
        defCols()
        filas()
        bind()
        'Dim i As Int32 = DataGrid1.Height.Value + 80 + Int32.Parse(Table1.Style.Item("TOP").Substring(0, 3))
    End Sub
    Sub defCols()
        'DataGrid1.Visible = False
        Dim i As Int32, colNames() As String
        If lengua = "es" Then
            Dim cn() As String = {" ", "Descripción", "Descarga"}
            colNames = cn
        ElseIf lengua = "ca" Then
            Dim cn() As String = {" ", "Descripció", "Descàrrega"}
            colNames = cn
        ElseIf lengua = "fr" Then
            Dim cn() As String = {" ", "Description", "Download"}
            colNames = cn
        ElseIf lengua = "de" Then
            Dim cn() As String = {" ", "Beschreibung", "Download"}
            colNames = cn
        ElseIf lengua = "it" Then
            Dim cn() As String = {" ", "Descrizione", "Download"}
            colNames = cn
        ElseIf lengua = "pt" Then
            Dim cn() As String = {" ", "Descrição", "Download"}
            colNames = cn
        Else
            Dim cn() As String = {" ", "Description", "Download"}
            colNames = cn
        End If
        For i = 0 To colNames.Length - 1
            Dim col1 As DataColumn = New DataColumn("Int32Col")
            col1.ColumnName = colNames(i)
            col1.DataType = System.Type.GetType("System.String")
            dt.Columns.Add(col1)
        Next

    End Sub
    Sub filas()
        Dim i As Int32
        Dim f() As Int32 = {8, 7, 2, 0, 1, 9, 10, 11}
        'Dim f() As Int32 = {8, 7, 6, 5, 2, 0, 1}
        'Dim f2() As Int32 = {6, 5, 0, 1, 2}
        'If lengua = "ca" Then f = f2
        Dim simg As String = " style=""width:570px;"" "
        Dim sImgDownload As String = "<img runat='server' runat='server' src='/images/download.bmp' border='0' onmouseover=""this.src='/images/download2.bmp'"" onmouseout=""this.src='/images/download.bmp'"">"
        For i = 0 To f.Length - 1
            Dim dr2 As DataRow = dt.NewRow
            dr2(0) = (i + 1).ToString()
            lengua = "en"
            Select Case lengua
                Case "ca"
                    Select Case f(i)
                        Case 0
                            dr2(1) = "<p>Winfonts és per a la visualització de fonts de caràcters (instal·lades o no a Windows).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/WinFonts.png' border='0' />"
                        Case 1
                            dr2(1) = "<p>Winranking explora amb una cerca diversos cercadors " _
                            + "i permet coneixar en quin número de pàgina es troba una url (o un text).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/winranking.jpg' border='0' />"
                        Case 3
                            'dr2(1) = "<p>Programari lliure de contabilitat.</p>" + _
                            '"<img runat='server' src='/images/contajunfor.jpg' border='0' />"
                        Case 4
                            dr2(1) = "<p>&nbsp;</p><p>Programari lliure per a impressos de factura.</p><p>&nbsp;</p>"
                    End Select
                Case "es"
                    Select Case f(i)
                        Case 0
                            dr2(1) = "<p>Winfonts permite ver las fuentes de carácteres (instaladas o no en Windows).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/WinFonts.png' border='0' />"
                        Case 1
                            dr2(1) = "<p>Winranking explorar, con una determinada clave de búsqueda, diversos buscadores " +
                            "y permite conocer en qué número de página aparece una url (o también un texto).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/winranking.jpg' border='0' />"
                    End Select
                Case "fr"
                    Select Case f(i)
                        Case 0
                            dr2(1) = "<p>Winfonts c'est per la visulisation de jeu de caractères de caractères (installé ou pas dans Windows).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/WinFonts.png' border='0' />"
                        Case 1
                            dr2(1) = "<p>Winranking explore, avec une clef donnée de recherche, et permet sait dans quel nombre de page est une url donnée (ou un texte). </p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/winranking.jpg' border='0' />"
                    End Select
                Case "de"
                    Select Case f(i)
                        Case 0
                            dr2(1) = "<p>Winfonts, programm fur die visalisation der Charaktere (installiert oder nicht in Windows)</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/WinFonts.png' border='0' />"
                        Case 1
                            dr2(1) = "<p>Winranking erforscht, mit einer gegebenen Suchentaste, und ermöglicht wissen in welche Anzahl eine gegebene Url (oder Text) ist.</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/winranking.jpg' border='0' />"
                    End Select
                Case "it"
                    Select Case f(i)
                        Case 0
                            dr2(1) = "<p>Winfonts, programma per mostrare di stili di carattere (installati o no a Windows).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/WinFonts.png' border='0' />"
                        Case 1
                            dr2(1) = "<p>Winranking esplora, con una chiave di ricerca data, diversi motori" +
                            " di ricerca e consente sapere in cui numero di pagina è un url dato (o un testo).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/winranking.jpg' border='0' />"
                    End Select
                Case "pt"
                    Select Case f(i)
                        Case 0
                            dr2(1) = "<p>Winfonts, programa exibindo as fontes de caráteres (instalaram ou não em Windows).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/WinFonts.png' border='0' />"
                        Case 1
                            dr2(1) = "<p>Winranking explora, com uma tecla dada de pesquisa, vários motores de pesquisa " +
                            " e permite sabem em que número de página é um url dado (ou texto).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/winranking.jpg' border='0' />"
                    End Select
                Case Else
                    Select Case f(i)
                        Case 0
                            dr2(1) = "<p>Winfonts displays fonts of characters (installed or not in Windows).</p>" +
                            "<img runat='server' " + simg + "  width='570px' src='/images/WinFonts.png' border='0' />"
                            'Case 1
                            '    dr2(1) = "<p>Winranking explores several search engines, " + _
                            '    "given a search key for the search engines " + _
                            '    " and a word(s), for ex. a domain or a text," + _
                            '    " to look for in the resulting pages " + _
                            '    " of the search engine " + _
                            '    "and reports the page numbers in which the word(s) appear(s). " + _
                            '    "The search engines list is configured " + _
                            '    "in an attached text file (not documented).</p>" + _
                            '    "<img runat='server' " + simg + "  width='570px' src='/images/winranking.jpg' border='0' />"
                    End Select
            End Select
            Select Case lengua
                Case "ca"
                    Select Case f(i)
                        Case 0
                            dr2(2) = "<a href='download.aspx?file=winfont'>Descàrrega</a>"
                        Case 1
                            dr2(2) = "<a href='download.aspx?file=winranking'>Descàrrega</a>"
                        Case 3
                            dr2(2) = "<a href='download.aspx?file=contajunfor'>Descàrrega (2009/04/28)</a>"
                            'dr2(2) += "<br><br><a href='download.aspx?file=codicj'>Codi font</a>"
                        Case 4
                            dr2(2) = "<a href='download.aspx?file=frajunfor'>Descàrrega (2009/04/28)</a>"
                            'Case 5
                            '    dr2(1) = "<a name=""Mates6"" /><p style=""align:center"">Mates6</p>"
                            '    dr2(1) += "<p>Processador y evaluador de expresions matemàtiques en VB.NET; fa ús de l'algoritme Shunting Yard i notació RPN."
                            '    dr2(1) += "<br />Download del codi font complet.</p><br />"
                            '    dr2(1) += "<a href='download.aspx?file=mates6'><img runat='server' " + simg + "  border='0' src='/images/testmates6.jpg' /></a>"
                            '    dr2(1) += "<br />"
                            '    dr2(2) = "<a href='download.aspx?file=mates6'>Download</a>"
                            'Case 6
                            '    dr2(1) = "<a name=""Mates6_2"" /><p style=""align:center"">Mates6_2</p>"
                            '    dr2(1) += "<p>Calculadora polinomial, codi font complet, en VB.NET; fa ús de l'algoritme Shunting Yard i notació RPN."
                            '    dr2(1) += "<br />Download del codi font complet.</p><br />"
                            '    dr2(1) += "<p>(Versions antigues:"
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2'>v2.0.1.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.2.0'>v2.0.2.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.3.0'>v2.0.3.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.4.0'>v2.0.4.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.5.0'>v2.0.5.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.5.0'>v2.0.6.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.6.0'>v2.0.6.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.8.0'>v2.0.8.0</a>) "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.9.0'>v2.0.9.0</a>) "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.10.0'>v2.0.10.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.0'>v2.0.11.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.2'>v2.0.11.2</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.2'>v2.0.11.4</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.4'>v2.0.11.5</a>)</p>"
                            '    dr2(1) += "<a alt=""Últim: v3.0.02"" href='download.aspx?file=mates6_3.0.02'><img runat='server' " + simg + "  border='0' src='/images/testmates6_2.jpg' /></a>"
                            '    dr2(1) += "<br />"
                            '    dr2(2) = "<a alt=""Última versió"" href='download.aspx?file=mates6_3.0.02'>Download<br />última<br />versió</a>"
                            '    dr2(2) += "<br /><br />"
                            '    dr2(2) += "<a alt=""codi font"" href='download.aspx?file=mates6_source_3.0.02'>Download<br />últim<br />codi font</a>"
                    End Select
                Case "es"
                    Select Case f(i)
                        Case 0
                            dr2(2) = "<a href='download.aspx?file=winfont'>Descargar</a>"
                        Case 1
                            dr2(2) = "<a href='download.aspx?file=winranking'>Descargar</a>"
                            ''Case 5
                            ''    dr2(1) = "<a name=""Mates6"" /><p style=""align:center"">Mates6</p>"
                            ''    dr2(1) += "<p>Procesador y evaluador de expresiones matemáticas en VB.NET; usa el algoritmo Shunting Yard y notación RPN."
                            ''    dr2(1) += "<br />Download del código fuente completo.</p><br />"
                            ''    dr2(1) += "<a href='download.aspx?file=mates6'><img runat='server' " + simg + "  border='0' src='/images/testmates6.jpg' /></a>"
                            ''    dr2(1) += "<br />"
                            ''    dr2(2) = "<a href='download.aspx?file=mates6'>Download</a>"
                            ''Case 6
                            ''    dr2(1) = "<a name=""Mates6_2"" /><p style=""align:center"">Mates6_2</p>"
                            ''    dr2(1) += "<p>Calculadora polinomial, codigo fuente completo, en VB.NET; usa el algoritmo Shunting Yard i notación RPN."
                            ''    dr2(1) += "<br />Download del codigo fuente completo.</p><br />"
                            ''    dr2(1) += "<p>(Versiones antiguas: "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2'>v2.0.1.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.2.0'>v2.0.2.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.3.0'>v2.0.3.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.4.0'>v2.0.4.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.5.0'>v2.0.5.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.6.0'>v2.0.6.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.8.0'>v2.0.8.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.9.0'>v2.0.9.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.0'>v2.0.11.0</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.2'>v2.0.11.2</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.4'>v2.0.11.4</a> "
                            ''    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.5'>v2.0.11.5</a>)</p>"
                            ''    dr2(1) += "<a alt=""Última: v2.0.11.10"" href='download.aspx?file=mates6_2.0.11.10'><img runat='server' " + simg + "  border='0' src='/images/testmates6_2.jpg' /></a>"
                            ''    dr2(1) += "<br />"
                            ''    dr2(2) = "<a alt=""Última versión"" href='download.aspx?file=mates6_2.0.11.10'>Download<br />última<br />versión</a>"
                            ''    dr2(2) += "<br /><br />"
                            ''    dr2(2) += "<a alt=""código fuente"" href='download.aspx?file=mates6_source_2.0.11.10'>Download<br />último<br />código fuente</a>"
                    End Select
                Case Else
                    Select Case f(i)
                        Case 0
                            dr2(2) = "<a href='download.aspx?file=winfont'>Download</a>"
                        'Case 1
                        '    dr2(2) = "<a href='download.aspx?file=winranking'>Download</a>"
                            'Case 5
                            '    dr2(1) = "<a name=""Mates6"" /><p style=""align:center"">Mates6</p>"
                            '    dr2(1) += "<p>Free math parser and evaluator in VB.NET; uses Shunting Yard algorithm and RPN notation."
                            '    dr2(1) += "<br />Full source code download.</p>"
                            '    dr2(1) += " If you have any comments or suggestions, please leave them "
                            '    dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.<br />"
                            '    dr2(1) += "<a href='download.aspx?file=mates6'><img runat='server' " + simg + "  border='0' src='/images/testmates6.jpg' /></a>"
                            '    dr2(1) += "<br />"
                            '    dr2(2) = "<a href='download.aspx?file=mates6'>Download</a>"
                            'Case 6
                            '    dr2(1) = "<a name=""Mates6_2"" /><p style=""align:center"">Mates6_2 & Mates6_3</p>"
                            '    dr2(1) += "<p>Polynomial calculator, coded in VB.NET; employs Shunting Yard algorithm and RPN (postfix) notation."
                            '    dr2(1) += "<br />Newer version employs an abstract syntax tree (AST) instead, "
                            '    dr2(1) += "does matrix operations (also performs Jacobian), multivariable polynomial division, Lagrangian interpolation and resolves "
                            '    dr2(1) += "linear and non linear system of equations  (see examples combo box in the application's form)."
                            '    dr2(1) += "<br />Full source code download.</p>"
                            '    dr2(1) += " If you have any comments or suggestions, please leave them "
                            '    dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.<br />"
                            '    dr2(1) += "<p>(Older versions:"
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2'>v2.0.1.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.2.0'>v2.0.2.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.3.0'>v2.0.3.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.4.0'>v2.0.4.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.5.0'>v2.0.5.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.6.0'>v2.0.6.0</a><br />"
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.8.0'>v2.0.8.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.9.0'>v2.0.9.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.10.0'>v2.0.10.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.0'>v2.0.11.0</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.2'>v2.0.11.2</a><br />"
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.2'>v2.0.11.4</a> "
                            '    dr2(1) += "<a href='download.aspx?file=mates6_2.0.11.4'>v2.0.11.5</a>&nbsp;<br />"
                            '    dr2(1) += "<a href='download.aspx?file=mates6_3.0.20'>v3.0.20</a>)</p>"
                            '    dr2(1) += "<iframe width=""560"" height=""315"" src=""https://www.youtube.com/embed/6p4XRESox4A"" frameborder=""0"" allowfullscreen></iframe>"
                            '    'dr2(1) += "</object>"
                            '    ' <iframe width="560" height="315" src="https://www.youtube.com/embed/6p4XRESox4A" frameborder="0" allowfullscreen></iframe>
                            '    'dr2(1) += "</object>"
                            '    dr2(1) += "<a alt=""Latest: v3.0.20"" href='download.aspx?file=mates6_3.0.20'><img runat='server' " + simg + "  border='0' src='/images/testmates6_2.jpg' /></a>"
                            '    dr2(1) += "<br />"
                            '    dr2(2) = "<a alt=""Latest version"" href='download.aspx?file=mates8_3.0.20'>Latest<br />version<br />Download</a>"
                            '    dr2(2) += "<br /><br />" 
                            '    dr2(2) += "<a alt=""source code"" href='download.aspx?file=mates6_source_3.0.20'>Latest<br />source code<br />Download</a>"
                        Case 7
                            dr2(1) = "<a name=""polycalcinexcel"" /><h3 style=""align:center"">Polynomial Calculator in Excel Workbook</h3>"
                            dr2(1) += "<p>Polynomial calculator, coded in VB.NET; Renewed math parser and evaluator, extensive use of Regex."
                            dr2(1) += " Free source code contains Complex, Vector, Matrix and more revised classes."
                            dr2(1) += " Full source code download.</p>"
                            dr2(1) += "Last update: 2021/02/23 (v8.3.192)<br />"
                            dr2(1) += "<p><a href='download.aspx?file=polycalcinexcel'><img runat='server' alt=""Excel Workbook.""  width='570px' src=""/images/matesExcel1.jpg""  border=""0"" /></a></p>"
                            dr2(1) += "<br /><br />"
                            dr2(1) += "<p><a href='download.aspx?file=polycalcinexcel'><img runat='server' alt=""Excel Workbook""  width='570px' src=""/images/matesExcel2.jpg""  border=""0"" /></a></p><br />"
                            dr2(1) += " If you have any comments or suggestions, please leave them "
                            dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.<br />"
                            dr2(2) = "<a alt=""Latest version"" href='download.aspx?file=polycalcinexcel'>Download</a>"
                            dr2(2) += "<br /><br />"
                            dr2(2) += "<a alt=""source code"" href='download.aspx?file=mates8_excel'>Source code<br />Download</a>"
                        Case 8
                            Dim sP As String = "<p style=""margin-bottom:4px;text-align:justify;margin-left:20px;margin-right:15px;line-indent:15px;line-height:1.125"">"
                            dr2(1) = "<a name=""mates8"" /><h3 style=""align:center"">Polynomial & Math Calculator: Mates8</h3>"
                            dr2(1) += "<h4 style=""align:center"">Mathematical expression parser and evaluator</h3>"
                            dr2(1) += "<span =""font-family:Calibri; size:small;"">" +
                                        sP +
                                    " &nbsp;&nbsp;Mates8 is the downloadable version of the Online Polynomial and" +
                                    " Scientific Calculator.</p>" +
                                    sP + "Mates8 features: <br /><br />" +
                                "» Basic arithmetic and polynomial operations (-,+,*,/,^,%), including GCD <br />" +
                                "» finds polynomials roots, of real or complex coefficients,  <br />" +
                                "» does multivariable polynomial division, <br />" +
                                "» Lagrangian interpolation and <br />" +
                                "» basic matrix operations (-,+,*,/,^). <br />" +
                                "» May obtain the inverse matrix,  <br />" +
                                "» cofactor, <br />" +
                                "» adjoint, <br />" +
                                "» transpose matrix, <br />" +
                                "» echelon form, <br />" +
                                "» Jordan form, <br />" +
                                "» eigenvalues, <br />" +
                                "» eigenvectors, and <br />" +
                                "» the determinant of an invertible matrix of real, complex, polynomial or expression entries. <br />" +
                                "» Performs derivatives using Euler's notation, e.g.: Dx(sin(x)); <br />" +
                                "» Jacobian; <br />" +
                                "» approximates the exponential matrix (e.g. exp(1,cos(x)|0,1)=(2.718..,2.718...*cos(x)|0,2.718...)) <br />" +
                                "» Resolves linear, when compatible and determinate or indeterminate, <br />" +
                                "» as non linear system of equations. <br />" +
                                "» Also, is capable of bounded definite integrals, <br />" +
                                "» inmediate, <br />" +
                                "» trigonometric or <br />" +
                                "» partial fractions integration -quotients of polynomials integration- and <br />" +
                                "» integration by parts. <br />" +
                                "» Solve some ordinary first order differential equations with or without specifying the initial condition. <br />" +
                                "» Approximates first order diff. eqs. by Euler's method specifying the initial condition. <br />" +
                                "</span>"
                            '" &nbsp;&nbsp;Mates8 does basic math and polynomial operations (-,+,*,/,^,%)," + _
                            '" finds polynomials roots, of real or complex coefficients," + _
                            '" multivariate polynomial division, Lagrangian interpolation" + _
                            '" and basic matrix operations (-,+,*,/,^). May obtain the inverse," + _
                            '" cofactor, adjoint and transpose " + _
                            '" matrix and the determinant of an invertible matrix of real, complex," + _
                            '" polynomial or expression entries." + _
                            '" Finds eigenvalues, eigenvectors and Jordan form of a matrix." + _
                            '" Performs derivatives using Euler's notation, e.g.: Dx(sin(x)); Jacobian;" + _
                            '" (approximation to) exponential matrix (e.g. exp(1,cos(x)|0,1)=(2.718..,2.718...*cos(x)|0,2.718...)); " + _
                            '" resolves linear, when compatible and determinate, as non linear system of equations" + _
                            '" and may solve some ordinary first order differential equations with or without" + _
                            '" specifying the initial condition.</p>" + _
                            'sP + _
                            '" &nbsp;&nbsp;Also is capable of bounded definite integrals, inmediate, trigonometric" + _
                            '" or partial fractions integration -quotients of polynomials integration- " + _
                            '" and integration by parts.</p> " + _
                            'sP + _
                            '" &nbsp;&nbsp;You may find examples in Predefined/Examples menu option of the application's form.</p>" + _
                            '"</font>"
                            dr2(1) += "<p style=""text-align:left;margin-left:20px;margin-right:15px"">&nbsp;&nbsp;Mates8 intends to be an improved version" +
                                    " of previous Mates2-Mates6 calculators.</p>"
                            dr2(1) += "<p style=""text-align:left;margin-left:20px;margin-right:15px"">"
                            dr2(1) += "&nbsp;&nbsp;Full (and free) source code download.<br />"
                            dr2(1) += "&nbsp;&nbsp;If you have any comments or suggestions, please leave them "
                            dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.</p><br />"
                            dr2(1) += "<img runat='server' alt=""mates8""  border='0'  src='/images/mates8a.png' /><br /><br />"
                            dr2(1) += "<img runat='server' alt=""mates8""  border='0'  src='/images/mates8b.png' /><br /><br />"
                            dr2(1) += "<img runat='server' alt=""mates8""  border='0'  src='/images/mates8c.png' /><br /><br />"
                            dr2(1) += "<img runat='server' alt=""mates8""  border='0'  src='/images/mates8d.png' /><br /><br />"

                            dr2(1) += "<div style=""margin:10px 10px 10px 10px"">"
                            dr2(1) += "<div style=""border:solid black 1px;margin:10px 10px 10px 10px;font-size:small;font-family:Calibri,Arial;vertical-align:middle;"">"
                            dr2(1) += "Updated: 2022/03/05 <br />"
                            dr2(1) += Me.sLIdownload("mates8v8.4.194", "Polynomial & Scientific Calculator (v8.4.194)")
                            dr2(1) += "<div>&nbsp;</div>"
                            dr2(1) += Me.sLIdownload("mates8v8.3.192source", "Polynomial & Scientific Calculator's full source code (v8.3.192)")
                            dr2(1) += "<div>&nbsp;</div>"
                            dr2(1) += "</div>"
                            dr2(1) += "</div>"

                            dr2(1) += "<div style=""margin:10px 10px 10px 10px"">"
                            dr2(1) += "<div style=""border:solid black 1px;margin:10px 10px 10px 10px;font-size:small;font-family:Calibri,Arial;vertical-align:middle;"">"
                            dr2(1) += "Updated: 2023/05/06 <br />"
                            dr2(1) += Me.sLIdownload("math10_exe", "Math10 Computer Algebra System (CAS calculator)")
                            dr2(1) += "<div>&nbsp;</div>"
                            dr2(1) += Me.sLIdownload("math10_src", "Math10 CAS Calculator full source code")
                            dr2(1) += "<div>&nbsp;</div>"
                            dr2(1) += "</div>"
                            dr2(1) += "</div>"


                            dr2(1) += "<div style=""margin:10px 10px 10px 10px"">"
                            dr2(1) += "<div style=""border:solid black 1px;margin:10px 10px 10px 10px;font-size:small;font-family:Calibri,Arial"">"
                            dr2(1) += "Updated: 2021/02/23 (v8.3.192)<br />"
                            dr2(1) += Me.sLIdownload("mates8NetLib", "Polyn. & Sci. Calculator's .dll library for .Net programming")
                            dr2(1) += "<div style=""margin-left:10px;position:relative;top:-9px;"">" +
                                                     "Also, contains .tbl file for Visual Basic 6.</div>"
                            dr2(1) += "<div>&nbsp;</div>"
                            dr2(1) += Me.sLIdownload("testNetMates8", "Test Project for .Net programming")
                            dr2(1) += "<div>&nbsp;</div>"
                            dr2(1) += "</div>"
                            dr2(1) += "</div>"




                            dr2(1) += "<div style=""margin:10px 10px 10px 10px"">"
                            dr2(1) += "<div style=""border:solid black 1px;margin:10px 10px 10px 10px;font-size:small;font-family:Calibri,Arial"">"
                            dr2(1) += Me.sLIdownload("convert2rpn", "to RPN converter (ASP.NET)")
                            dr2(1) += "</div>"
                            dr2(1) += "</div>"





                            'dr2(1) += "<br />"
                            'dr2(1) += "<br />"

                            dr2(2) = "<a alt=""Polyn.& Sci. Calculator"" href='download.aspx?file=mates8v8.3.192'>Polyn. & Sci. Calc. Download</a>"
                        Case 9
                            dr2(1) = "<a name=""timezones"" /><h3 style=""align:center"">Timezones</h3>"
                            dr2(1) += "<p>Current local time and date in every time zone."
                            dr2(1) += "<br />Last update: 2014/04/29 </p>"
                            dr2(1) += "<p>Add a new clock (time zone) by selecting a row."
                            dr2(1) += "Select a row (i.e. the entire row, not a specific column) clicking on the left-most of the row."
                            dr2(1) += " Multiple rows selection is possible clicking while control key (or shift key) is pressed: "
                            dr2(1) += " just like in Windows' Explorer (https://windows.microsoft.com/en-us/windows/select-multiple-files-folders#1TC=windows-7)"
                            dr2(1) += " Works in any Windows version >= ""Win2000"". For versions prior to XP SP2 be sure .NETv2.0 framework is installed or install the framework."
                            dr2(1) += "<p><a href='download.aspx?file=timezones'><img runat='server' alt=""Timezones image"" width='570px' src=""/images/clock.png""  border=""0"" /></a></p>"
                            dr2(1) += " If you have any comments or suggestions, please leave them "
                            dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.<br />"
                            dr2(2) += "<a alt=""source code"" href='download.aspx?file=timezones'>Download</a>"
                        Case 10
                            dr2(1) = "<a name=""reports"" /><h3 style=""align:center"">PdfSharp and Excel reporting sample</h3>"
                            dr2(1) += "<p style=""text-align:center"">A VStudio Express 2013 for Web project</p>"
                            dr2(1) += "<p style=""text-align:justify"">The .vb file names ending in ""pdf"" invoke pdfsharp.dll. "
                            dr2(1) += "The ones ending in ""xls"" correspond to the ""pdf"" ones with same "
                            dr2(1) += "sufix and the output format is an Excel Workbook. "
                            dr2(1) += " Note there are two font files (.ttf) to "
                            dr2(1) += " ensure the .pdf file contains embedded the required font. "
                            dr2(1) += "You'll need to change the paths, the web "
                            dr2(1) += "reference, fields and anything else required to your needs.</p>"
                            dr2(1) += " If you have any comments or suggestions, please leave them "
                            dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.<br />"
                            dr2(2) += "<a alt=""PdfSharp and Excel Reporting project download"" href='download.aspx?file=informes'>Download</a>"
                        Case 11
                            dr2(1) = "<a name=""wordmacros"" /><h3 style=""align:center"">Execute Macros In Word Documents In Folder</h3>"
                            dr2(1) += "<p>Execute existent Macros in all Word Documents that are placed inside a same folder."
                            dr2(1) += "<div onclick=""toggle('hider')"" style=""color:blue;"">Click here to show App's Help menu</div>"
                            dr2(1) += "<div id=""hider"" style=""display:none;text-align:left"" >"
                            Dim str1 As String = My.Resources.String1
                            dr2(1) += "<pre>" + str1 + "</pre>"
                            dr2(1) += "</div>"
                            dr2(1) += "<br />Last update: 2022/02/21</p>"
                            dr2(1) += " If you have any comments or suggestions, please leave them "
                            dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.<br />"
                            dr2(1) += "<img runat='server' alt=""extra parameters""  border='0'  src='/images/extra_parameters.png' /><br /><br />"
                            dr2(2) += "<a alt=""source code"" href='download.aspx?file=wordmacros'>Download</a>"
                    End Select
            End Select
            If f(i) = 2 Then
                dr2(1) = "<p style=""align:center"">Windipoles</p>"
                dr2(1) += "<p style=""align:justify"">Windipoles is a free passive filter simulator."
                dr2(1) += " It is a Windows .NET executable, so no software installation is required and works in Win32 and Win64 machines."
                dr2(1) += " It enables design, analysis, optimizations and plots, all graphically. "
                dr2(1) += " While component values are changed, graphic windows are updated instantly."
                dr2(1) += " Graphics may show, overlapped, more than one network for comparing purposes."
                dr2(1) += " Also, synthesizes Butterworth, Bessel, Chebyshev (types I & II), and Cauer (elliptic) filters."
                dr2(1) += " Windipoles has a help .chm file. Also it does not connect to internet and needs no special rights to execute."
                dr2(1) += " <br />Current version is v4.8.8.zip.<br />"
                dr2(1) += " If you have any comments or suggestions, please leave them "
                dr2(1) += "<a href='https://xrjunque.nom.es/commentfrm.aspx'>here</a>.<br />"
                dr2(1) += "<br />" '"<a href=""https://www.herostechnology.co.uk/pages/windipoles.html"">More details.</a></p>"
                dr2(1) += "<a href='download.aspx?file=windipoles'><img runat='server' " + simg + "  src='/images/windipoles.png' border='0' /></a></p>"
                Dim e3 As String = vbCrLf + "<table border='0' cellpadding='1' cellspacing='0' bgcolor='#000000'>" + vbCrLf +
                    "<tr><td>" + vbCrLf +
                    "	<table border='0' cellpadding='3' cellspacing='0' bgcolor='#FFFFCC'>" + vbCrLf +
                    "	<tr><td>" + vbCrLf +
                    "		<table border='1' cellpadding='1' cellspacing='0' bgcolor='brown' width='100%'>" + vbCrLf +
                    "		<tr><td>" + vbCrLf +
                    "			<table border='0' width='100%' cellpadding='1' cellspacing='0' bgcolor='brown'>" + vbCrLf +
                    "			<tr><td align=center><strong><font face='arial,helvetica,veradna' size=-1 color='#FFFFFF'>" + vbCrLf +
                    "				Rate Windipoles </font></strong>" + vbCrLf +
                    "			</td></tr>" + vbCrLf +
                    "			</table>" + vbCrLf +
                    "		</td></tr>" + vbCrLf +
                    "		</table>" + vbCrLf +
                    "	</td></tr>" + vbCrLf +
                    "	<tr><td align='center' valign='top' bgcolor='#FFFFCC'>" + vbCrLf +
                    "		<form  method='POST'  action='https://www.dxzone.com/cgi-bin/dir/rate.cgi'>" + vbCrLf +
                    "		<input type='hidden' name='ID' value='22062'>" + vbCrLf +
                    "		<a href='https://www.dxzone.com'>" + vbCrLf +
                    "		<img runat='server' src='https://www.dxzone.com/catalog/dxzonerate.gif' valign=top vspace=3 width=159 height=39 border='0'  alt='The DXZone.com'></a><br />" + vbCrLf +
                    "		<select name='rate' size='1'>" + vbCrLf +
                    "		<option>--- </option>" + vbCrLf +
                    "		<option>1 </option>" + vbCrLf +
                    "		<option>2 </option>" + vbCrLf +
                    "		<option>3 </option>" + vbCrLf +
                    "		<option>4 </option>" + vbCrLf +
                    "		<option>5 </option>" + vbCrLf +
                    "		<option>6 </option>" + vbCrLf +
                    "		<option>7 </option>" + vbCrLf +
                    "		<option>8 </option>" + vbCrLf +
                    "		<option>9 </option>" + vbCrLf +
                    "		<option>10 </option>" + vbCrLf +
                    "		</select><input type='Submit' value='Rate it'>" + vbCrLf +
                    "		<br /><font face='arial,verdana' size='-2'> (with 10 = top)</a>" + vbCrLf +
                    "		</form>" + vbCrLf +
                    "	</td></tr>" + vbCrLf +
                    "	</table>" + vbCrLf +
                    "</td></tr>" + vbCrLf +
                    "</table>" + vbCrLf
                e3 = "<a href=""https://www.dxzone.com/cgi-bin/dir/rate.cgi?ID=22062"">Rate Windipoles @ dxzone.com</a><br />"
                dr2(1) += e3
                dr2(2) = "<br /><br /><a href='download.aspx?file=windipoles'>Download</a>"
            ElseIf f(i) = 0 Then
                Dim posImg As Int32 = InStr(dr2(1), "<img")
                If posImg > 0 Then dr2(1) = Mid(dr2(1), 1, posImg - 1) + "Ver. 1.0.2<br />" + Mid(dr2(1), posImg)
            End If
            If (f(i) - 8) * (f(i) - 1) * (f(i) - 2) * (f(i) - 5) * (f(i) - 6) * (f(i) - 7) * (f(i) - 9) * (f(i) - 10) * (f(i) - 11) <> 0 Then
                ' Sourround "<img.." tag by the download anchor:
                Dim posImg As Int32 = InStr(dr2(1), "<img")
                Dim posEnd As Int32 = InStr(posImg, dr2(1), ">")
                Dim posEndAnchor As Int32 = InStr(dr2(2), ">")
                Dim sAnchorStart As String = Mid(dr2(2), 1, posEndAnchor)
                Dim posCloseAnchor As Int32 = InStr(posEndAnchor + 1, dr2(2), "<")
                Dim sAnchorClose As String = Mid(dr2(2), posCloseAnchor)
                Dim sStart As String = ""
                If posImg > 1 Then
                    sStart = Mid(dr2(1), 1, posImg - 1)
                End If
                Dim sEnd As String = ""
                If posEnd < Len(dr2(1)) Then
                    posEnd = Mid(dr2(1), posEnd + 1)
                End If
                dr2(1) = sStart + sAnchorStart + Mid(dr2(1), posImg, posEnd - posImg + 1) + "</a>" + sEnd
            End If
            dt.Rows.Add(dr2)
        Next
    End Sub
    Function sLIdownload(ByVal sFile As String, ByVal txt As String) As String
        Dim e1 As String = ""
        Dim sImgDownload As String = "<img runat='server' src='/images/download.bmp' border='0'  "
        sImgDownload += " alt=""Download: " + txt + """"
        sImgDownload += " title=""Download: " + txt + """"
        sImgDownload += " style=""width:103px;height:31px;"" "
        sImgDownload += " onmouseover=""this.src='/images/download2.bmp'"" "
        sImgDownload += " onmouseout=""this.src='/images/download.bmp'"" "
        sImgDownload += " onclick=""this.src='/images/download.bmp';this.onmouseout=img3;"" "
        sImgDownload += ">"
        'Dim sImgDownload As String = "<img runat='server' src='/images/download.bmp' border='0'>"
        Try
            e1 += "<div style=' font-family:Calibri; text-align: left; margin-left:10px; vertical-align:middle;'>"
            e1 += "<a style='myLink' href='download.aspx?file=" + sFile + "'>" ' mates8v8.3.53'>"
            e1 += sImgDownload
            e1 += "</a>"
            e1 += "<span style=""margin-left:10px;position:relative;top:-9px;"">" + txt + "</span>"
            e1 += "</div>"
        Catch ex As Exception

        End Try
        Return e1
    End Function
    Sub bind()
        Dim lit As New Literal
        Try
            Dim i As Int32
            Dim e1 As String = "<table style=""font-family:Calibri,Arial;font-size:large"">" + vbCrLf
            'e1 += "<tr style="""">" + vbCrLf
            'For i = 0 To dt.Columns.Count - 1
            '    If i = 0 Then
            '        e1 += "<td style=""width:35px"">" + dt.Columns(i).Caption + "</td>"
            '    Else
            '        e1 += "<td>" + dt.Columns(i).Caption + "</td>"
            '    End If
            'Next
            'e1 += vbCrLf + "</tr>" + vbCrLf
            For i = 0 To dt.Rows.Count - 1
                If i Mod 2 Then
                    'e1 += "<tr style="";font-weight:normal;text-align:justify"">" + vbCrLf
                    e1 += "<tr>" + vbCrLf
                Else
                    'e1 += "<tr style=""font-weight:normal;text-align:justify"">" + vbCrLf
                    e1 += "<tr>" + vbCrLf
                End If
                For j As Int32 = 0 To dt.Columns.Count - 1
                    If j = 0 Then
                        e1 += "<td style=""width:35px"" align='center'>"
                    Else
                        e1 += "<td align=""center"">"
                    End If
                    e1 += dt.Rows(i).Item(j)
                    If j = 1 Then e1 += "<hr width='80%' height='1' /><br />"
                    e1 += "</td>" + vbCrLf
                Next
                e1 += "</tr>" + vbCrLf
            Next
            e1 += "</table></center>" + vbCrLf
            lit.Text = e1
            PlaceHolder1.Controls.Clear()
            PlaceHolder1.Controls.Add(lit)
        Catch ex As Exception

        End Try
        'DataGrid1.Visible = True
        'DataGrid1.AutoGenerateColumns = False
        'dt.Columns.Item(1).ExtendedProperties.Add("WIDTH", Unit.Pixel(160))
        'Dim dv As DataView = dt.DefaultView
        'Dim u As New System.Web.UI.WebControls.Unit((dt.Rows.Count + 1) * 25)
        'DataGrid1.Height = u
        'DataGrid1.DataSource = dv
        'DataGrid1.DataBind()
        'DataGrid1.HeaderStyle.Wrap = True
    End Sub
    Sub logFile(ByVal ex As Exception, ByVal e1 As String, ByVal e2 As String)
        Try
            If Len(e1) Then
                e1 = Replace(e1, vbCrLf, "")
            End If
            If Len(e2) Then
                e2 = Replace(e2, vbCrLf, "")
            End If
            Dim f As FileStream
            f = New FileStream(MapPath("~\txt\polycalc.txt"), FileMode.Append)
            Dim dt As DateTime = Now
            Dim host As String = ""
            Try
                host = getHostName(Request.UserHostAddress)
            Catch ex1 As Exception

            End Try
            Dim fs As New StreamWriter(f, System.Text.Encoding.Unicode)
            fs.WriteLine(Request.UserHostAddress + Chr(9) _
            + host + Chr(9) + Now().ToShortDateString() + _
            Chr(9) + dt.ToShortTimeString() + Chr(9) + e1 + Chr(9) + e2)
            If Not (ex Is Nothing) Then
                Dim e3 As String = ""
                fs.WriteLine(Request.UserHostAddress + Chr(9) _
                + host + Chr(9) + Now().ToShortDateString() + _
                Chr(9) + dt.ToShortTimeString() + Chr(9) + _
                ex.Message + Chr(9) + "texbox1: " + e3)
            End If
            fs.Close()
            Try
                f.Close()
            Catch ex2 As Exception

            End Try
        Catch ex3 As Exception

        End Try
    End Sub
    Public Function getHostName(ByVal strIP As String) As String
        Dim myIP As System.Net.IPAddress = System.Net.IPAddress.Parse(strIP)
        Dim hostInfo As System.Net.IPHostEntry = _
        System.Net.Dns.GetHostEntry(myIP)
        Return hostInfo.HostName
    End Function
    'Private Sub language_SelectedIndexChanged(ByVal sender As System.Object, ByVal e As System.EventArgs) Handles language.SelectedIndexChanged
    '    If language.SelectedIndex >= 0 Then
    '        logFile(Nothing, "idioma=" + language.SelectedItem.Text, "")
    '        C_SessionWeb.idioma(nID) = language.SelectedValue
    '    End If
    'End Sub
End Class
