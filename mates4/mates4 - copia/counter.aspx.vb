Imports System.IO

Public Class counter
    Inherits System.Web.UI.Page

    Dim n As web
    Protected Sub btnGO_Click(sender As Object, e As System.EventArgs) Handles btnGO.Click
        Try
            Dim psw As String = Password1.Text
            If psw = "herostech" Then
                n = web.herostech
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
            f2 = New FileStream(
                MapPath(".\txt\herostech.txt"),
                FileMode.Open, IO.FileAccess.Read, IO.FileShare.ReadWrite)
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
            Dim vs2(-1) As String
            Do While Not sr.EndOfStream
                Dim vS() As String = Split(sr.ReadLine, vbTab)
                vs2 = vS
                If vS.Length < 3 Then GoTo sig
                Dim fch1 As Date
                Dim sD As String = vS(vInd(1))
                Dim b As Boolean
                If n <> web.comments Then
                    If n = web.dowloads Then
                        b =
                            Date.TryParseExact(sD, "dd/MM/yyyy",
                               Nothing,
                              System.Globalization.DateTimeStyles.None,
                                fch1)
                    Else
                        b =
                            Date.TryParseExact(sD, "yyyy/MM/dd   hh:mm:ss",
                               Nothing,
                              System.Globalization.DateTimeStyles.None,
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
                        cs(1).Text =
                         "<a href=""LatLong.aspx?ip=" + vS(0) + """>" + vS(0) + "</a>"
                        If n = web.herostech Then
                            cs(1).Text =
                         "<a href=""http://ip-api.com/" + vS(0) + """>" + vS(0) + "</a>"
                        End If
                        If n = web.dowloads Then
                            cs(1).Text =
        "<a href=""LatLong.aspx?ip=" + vS(1) + """>" + vS(1) + "</a>"
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
            Response.Write(s2)
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
