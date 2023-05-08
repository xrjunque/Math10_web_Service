Imports System.IO

Public Class contadores
    Inherits System.Web.UI.Page

    Dim n As web

    Protected Sub btnGO_Click(sender As Object, e As System.EventArgs) Handles btnGO.Click
        Try
            Dim psw As String = Password1.Text
            If psw = "xrjunque" Then
                n = web.xrjunque
                readCounterAndPopulate()
            ElseIf psw = "herostech" Then
                n = web.herostech
                readCounterAndPopulate()
            ElseIf psw = "roots" Then
                n = web.roots
                readCounterAndPopulate()
            ElseIf psw = "calc" Then
                n = web.calc
                readCounterAndPopulate()
            ElseIf psw = "down" Then
                n = web.dowloads
                readCounterAndPopulate()
            ElseIf psw = "com" Then
                n = web.comments
                readCounterAndPopulate()
            End If
        Catch ex As Exception

        End Try

    End Sub
    Sub readCounterAndPopulate()
        Try
            Dim ph As PlaceHolder = FindControl("PlaceHolder1")
            Dim fch As DateTime = Now
            Dim f2 As FileStream = Nothing
            With Calendar1
                fch = New DateTime(.SelectedDate.Ticks)
            End With
            If n = web.xrjunque Then
                f2 = New FileStream( _
                  "C:\inetpub\wwwroot\txt\calculadora.txt", FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
            ElseIf n = web.herostech Then
                f2 = New FileStream( _
                  "C:\inetpub\wwwroot\txt\herostech.txt", FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
            ElseIf n = web.calc Then
                f2 = New FileStream( _
                  "C:\inetpub\wwwroot\txt\polycalc.txt", FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
            ElseIf n = web.roots Then
                f2 = New FileStream( _
                  "C:\inetpub\wwwroot\txt\rootfinder.txt", FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
            ElseIf n = web.dowloads Then
                f2 = New FileStream( _
                  "C:\inetpub\wwwroot\txt\downloads.txt", FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
            ElseIf n = web.comments Then
                f2 = New FileStream( _
                  "C:\inetpub\wwwroot\txt\comments.txt", FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
            End If
            Dim sr As New StreamReader(f2, True)
            Dim tbl As New Table
            Dim nRow As Int32 = 0
            Dim vInd() As Int32 = {0, 2, 1, 3, 4, 5, 8, 7}
            If n = web.herostech Then
                vInd = New Int32() {0, 2, 1, 3, 4, 5, 6, 7}
                'vInd = New Int32() {0, 3, 2, 4, 5, 7, 8, 9}
            ElseIf n = web.dowloads Then
                vInd = New Int32() {0, 3, 2, 1, 4, 5, 6, 7}
            ElseIf n = web.xrjunque Then
                vInd = New Int32() {0, 2, 1, 3, 8, 4, 7, 5}
            Else
                vInd = New Int32() {0, 2, 1, 3, 5, 4, 6}
            End If
            Do While Not sr.EndOfStream
                Dim vS() As String = Split(sr.ReadLine, vbTab)
                If vS.Length < 3 Then GoTo sig
                Dim fch1 As Date
                Dim sD As String = vS(vInd(1))
                Dim b As Boolean
                If n <> web.comments Then
                    If n = web.dowloads Then
                        b = _
                            Date.TryParseExact(sD, "dd/MM/yyyy", _
                               Nothing, _
                              System.Globalization.DateTimeStyles.None, _
                                fch1)
                    Else
                        b = _
                            Date.TryParseExact(sD, "yyyy/MM/dd   hh:mm:ss", _
                               Nothing, _
                              System.Globalization.DateTimeStyles.None, _
                                fch1)
                    End If
                    If Not b Then GoTo sig
                    If fch1.Year <> fch.Year Then GoTo sig
                    If fch1.Month <> fch.Month Then GoTo sig
                    If fch1.Day <> fch.Day Then GoTo sig
                End If
                Dim tr As New TableRow
                Dim cs(vS.Length - 1) As TableCell

                cs(0) = New TableCell
                nRow += 1
                cs(0).Text = nRow.ToString
                For i As Int32 = 1 To vS.Length - 1
                    If i >= vS.Length Then
                        Exit For
                    End If
                    If i >= cs.Length Then
                        Exit For
                    End If
                    If i >= vInd.Length Then
                        Exit For
                    End If
                    If vInd(i) >= vS.Length Then
                        GoTo sig
                    End If
                    If i = 1 Then
                        cs(1) = New TableCell
                        If n = web.dowloads Then
                            cs(1).Text =
        "<a target=""_self"" href=""LatLong.aspx?ip=" + vS(1) + """>" + vS(1) + "</a>"
                        Else
                            cs(1).Text =
        "<a target=""_self"" href=""LatLong.aspx?ip=" + vS(0) + """>" + vS(0) + "</a>"
                        End If

                    ElseIf i < vInd.Length Then
                        If n = web.herostech AndAlso vInd(i) = 6 Then
                            'Dim e1() As String = Split(vS(6), "/")
                            'vS(6) = e1(e1.Length - 1)
                        End If
                        cs(i) = New TableCell
                        If vS(vInd(i)) IsNot Nothing Then
                            cs(i).Text = vS(vInd(i))
                        End If
                    Else
                        cs(i) = New TableCell
                        If vS(i) IsNot Nothing Then
                            cs(i).Text = vS(i)
                        End If
                    End If
                Next
sig:
                If cs IsNot Nothing Then
                    For i = cs.Length - 1 To 1 Step -1
                        If cs(i) IsNot Nothing Then
                            Exit For
                        Else
                            ReDim Preserve cs(i - 1)
                        End If
                    Next
                    tr.Cells.AddRange(cs)
                    tbl.Rows.Add(tr)
                End If
            Loop
            sr.Close()
            f2.Close()
            tbl.BorderColor = Drawing.Color.CadetBlue
            tbl.BorderStyle = BorderStyle.Solid
            tbl.BorderWidth = Unit.Pixel(1)
            tbl.Font.Name = "Tahoma"
            tbl.CellPadding = 1
            tbl.CellSpacing = 1
            tbl.GridLines = GridLines.Horizontal
            tbl.Width = 900
            ph.Controls.Add(tbl)
            ph.Visible = True
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            Dim s2() As String = Split(s1, vbCrLf)
        End Try

    End Sub
    Enum web
        xrjunque = 1
        herostech = 2
        calc = 3
        roots = 4
        dowloads = 5
        comments = 6
    End Enum
End Class
