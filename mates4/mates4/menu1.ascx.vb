Imports mates4.C_sessionWeb

Public Class menu1
    Inherits System.Web.UI.UserControl

    Dim sMn() As String = { _
        "Home", _
        "Roots finder", _
        "Poly.& Math.Calc.", _
        "Graph", _
        "Downloads", _
        "Links", _
        "Comments" _
        }
    Dim url() As String = {
            "default.aspx",
            "rootfinder.aspx",
            "polycalc.aspx",
            "graphic.aspx",
            "swdownload.aspx",
            "links.aspx",
            "commentfrm.aspx"
        }
    Dim vAltTitle() As String = { _
            "Home page", _
            "Root finder", _
            "Polynomial and Scientific Calculator", _
            "Graph", _
            "Downloads", _
            "Links", _
            "Comments and Suggestions" _
        }
    Dim vImgNames() As String = C_sessionWeb.vImgNames
    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Try
            'If Session.Item("l1") Is Nothing Then
            '    Session.Add("l1", 162)
            '    Session.Add("l2", "*")
            'End If
            C_sessionWeb.initMenu(Me.Page)
            populateMenu2()
        Catch ex As Exception

        End Try
    End Sub
    Sub populateMenu2()
        Try
            Dim e1 = Request.QueryString.Item("desktop")
            If Len(e1) Then
                e1 = "?desktop=" + e1
            End If
            Dim sPg As String = LCase(Page.Request.Url.AbsoluteUri)
            Dim i As Int32
            Dim ctr As UI.Control = Me.FindControl("Literal1")
            Dim lit As UI.WebControls.Literal =
                CType(ctr, WebControls.Literal)
            With lit
                .Text = ""
                Dim t As String = ""
                For i = 0 To sMn.Length - 1
                    If InStr(sPg, url(i)) = 0 Then
                        t += "<a href='" + url(i) + e1 + "'"
                        t += ">"
                    Else
                    End If
                    t += "<img border='0' "
                    t += " alt='" + Me.vAltTitle(i) + "' "
                    t += " title='Go to " + Me.vAltTitle(i) + " Page' "
                    If InStr(sPg, url(i)) = 0 Then
                        t += " src='/images/"
                        t += vImgNames(i)
                        t += "'"
                    Else
                        t += " src='/images/"
                        t += vImgNames(i + vImgNames.Length / 2)
                        t += "'"
                    End If
                    t += " />"
                    If InStr(sPg, url(i)) = 0 Then
                        t += "</a>"
                    End If
                Next
                .Text = t
            End With

        Catch ex As Exception
            Throw ex
        End Try
    End Sub

End Class

