Public Partial Class tutorialv8_4
    Inherits System.Web.UI.Page
    Dim vImg()() As String = {New String() _
                           {"intro.png", _
                           "page2.png", _
                           "page3.png", _
                           "page4.png", _
                           "page5.png", _
                           "page6.png" _
                           } _
                           , _
                            New String() _
                           {"page1.png", _
                           "page2.png", _
                           "page3.png" _
                           } _
                           , _
                            New String() _
                           {"page1.png", _
                           "page2.png", _
                           "page3.png", _
                           "page4.png", _
                           "page5.png", _
                           "page6.png", _
                           "page7.png" _
                           } _
                           , _
                            New String() _
                           {"page1.png", _
                           "page2.png", _
                           "page3.png", _
                           "page4.png", _
                           "page5.png", _
                           "page6.png" _
                           } _
                           }

    Dim vMn()() As String = {New String() _
                           {"Introduction", _
                           "2", _
                           "3", _
                           "4", _
                           "5", _
                           "6" _
                           } _
                           , _
                            New String() _
                           {"1", _
                           "2", _
                           "3" _
                           } _
                           , _
                            New String() _
                           {"1", _
                           "2", _
                           "3", _
                           "4", _
                           "5", _
                           "6", _
                           "7" _
                           } _
                           , _
                            New String() _
                           {"1", _
                           "2", _
                           "3", _
                           "4", _
                           "5", _
                           "6" _
                           } _
                           }

    Dim sDownload() As String = { _
                        "mates8v8.4.0", _
                        "mates8v8.4.1", _
                        "mates8v8.4.2", _
                        "mates8v8.4.3"}
    Dim sTutorialDownload() As String = { _
                        "tutorialv8_4_0", _
                        "", _
                        "tutorialv8_4_2", _
                        "tutorialv8_4_3"}
    Dim vVerLink() As WebControls.LinkButton
    Dim vPageLink() As WebControls.LinkButton
    Dim nID As Int32

    Private Sub tutorialv8_4_Init(sender As Object, e As EventArgs) Handles Me.Init
        Try
        Catch ex As Exception

        End Try
    End Sub

    Protected Sub Page_Load(ByVal sender As Object, ByVal e As System.EventArgs) Handles Me.Load
        Try
            'MenuClick(Menu1.Items(0), Nothing)

            Try
                C_SessionWeb.contador( _
                    Page, _
                    Request, _
                    Response)
            Catch ex2 As Exception

            End Try
            Try
                nID = C_SessionWeb.getID(Page)
                C_SessionWeb.sProvieneDe(nID) = Request.Url.AbsoluteUri
            Catch ex As Exception
            End Try

            If Not Page.IsPostBack Then
                populateVersions()
                populateNumPages()
                dspPage()
                'Session.Add("VersionCtrls", vVerLink)
                'Session.Add("PageCtrls", vPageLink)
            Else
                'vVerLink = Session("VersionCtrls")
                'vPageLink = Session("PageCtrls")
                populateVersions()
                populateNumPages()
            End If


            Try
                'Dim sb As String = String.Format("Tutor8_4_{0}Page{1}.aspx", curVersion, curPage)
                'Dim u As String = sb
                'C_SessionWeb.contador3(Me, Request, Response, sb, u)
            Catch ex As Exception

            End Try



        Catch ex As Exception
            Dim s As String = ex.ToString
        End Try
    End Sub
    Property curPage As Int32
        Get
            Dim i As Int32
            Try
                Dim o = HiddenFieldcurPage.Value
                If Len(o) AndAlso _
                Int32.TryParse(HiddenFieldcurPage.Value, i) Then
                Else
                    i = 0
                End If
            Catch ex As Exception

            End Try
            Return i
        End Get
        Set(value As Int32)
            Try
                Dim i As Int32
                Dim o = HiddenFieldcurPage.Value
                If Len(o) AndAlso _
                Int32.TryParse(HiddenFieldcurPage.Value, i) Then
                Else
                    i = 0
                End If
                If i <> value Then
                    HiddenFieldcurPage.Value = value
                    'populateNumPages()
                    Try
                        Dim sb As String = String.Format("Tutor8_4_{0}Page{1}.aspx", curVersion.ToString, (curPage + 1).ToString)
                        Dim u As String = sb
                        'C_SessionWeb.contador3(Page, Request, Response, sb, u)
                    Catch ex2 As Exception

                    End Try
                End If
            Catch ex As Exception

            End Try
        End Set
    End Property
    Property curVersion As Int32
        Get
            Dim i As Int32
            Try
                Dim o = HiddenFieldVersion.Value
                If Len(o) AndAlso _
                Int32.TryParse(HiddenFieldVersion.Value, i) Then
                Else
                    i = 2
                End If
            Catch ex As Exception

            End Try
            Return i
        End Get
        Set(value As Int32)
            Try
                Dim i As Int32
                Dim o = HiddenFieldVersion.Value
                If Len(o) AndAlso _
                Int32.TryParse(HiddenFieldVersion.Value, i) Then
                Else
                    i = 0
                End If
                If i <> value Then
                    i = value
                    HiddenFieldVersion.Value = value
                    curPage = 0
                    populateVersions()
                    populateNumPages()
                    dspPage()
                    Try
                        Dim sb As String = String.Format("Tutor8_4_{0}Page{1}.aspx", curVersion.ToString, (curPage + 1).ToString)
                        Dim u As String = sb
                        'C_SessionWeb.contador3(Page, Request, Response, sb, u)
                    Catch ex2 As Exception

                    End Try
                End If
            Catch ex As Exception

            End Try
        End Set
    End Property
    Sub populateVersions()
        Try

            PlaceHolder1.Controls.Clear()
            For i As Int32 = 0 To vMn.Length - 1
                Dim link As New WebControls.LinkButton
                link.CommandName = i.ToString
                link.Text = i.ToString '' el nombre de las versiones es 0, 1, 2, ... (v8.4.0, .1, .2, ....
                link.ID = "version" + i.ToString
                AddHandler link.Click, AddressOf linkVersionClick
                PlaceHolder1.Controls.Add(link)
                ReDim Preserve vVerLink(i)
                vVerLink(i) = link
                Dim ctrSpace As New Literal()
                ctrSpace.Text = "&nbsp;&nbsp;"
                PlaceHolder1.Controls.Add(ctrSpace)
            Next
            For j As Int32 = 0 To vVerLink.Length - 1
                If j = curVersion Then
                    vVerLink(j).Enabled = False
                    vVerLink(j).ForeColor = Drawing.Color.Red
                Else
                    vVerLink(j).Enabled = True
                    vVerLink(j).ForeColor = Drawing.Color.Black
                End If
            Next
        Catch ex As Exception
            Dim s1 As String = ex.ToString
        End Try
    End Sub
    Sub populateNumPages()
        Try
            If sTutorialDownload(curVersion).Length = 0 Then
                HyperLink2.Visible = False
            Else
                HyperLink2.Visible = True
                HyperLink2.NavigateUrl = "download.aspx?file=" + sDownload(curVersion)
                HyperLink2.Text = sDownload(curVersion) + " download"
            End If
            If sTutorialDownload(curVersion).Length = 0 Then
                HyperLink1.Visible = False
            Else
                HyperLink1.Visible = True
                HyperLink1.NavigateUrl = "download.aspx?file=" + _
                    sTutorialDownload(curVersion)
            End If
            PlaceHolderPagesNum.Controls.Clear()
            Dim i As Int32
            For i = 0 To vMn(curVersion).Length - 1
                Dim link As New WebControls.LinkButton
                link.CommandArgument = vMn(curVersion)(i).ToString
                link.CommandName = i.ToString
                link.ID = "page" + i.ToString
                link.Text = vMn(curVersion)(i).ToString
                link.Height = New UI.WebControls.Unit(30)
                AddHandler link.Click, AddressOf linkPagesClick
                PlaceHolderPagesNum.Controls.Add(link)
                ReDim Preserve vPageLink(i)
                vPageLink(i) = link
                Dim ctrSpace As New Literal()
                ctrSpace.Text = "&nbsp;&nbsp;"
                PlaceHolderPagesNum.Controls.Add(ctrSpace)
            Next
            For j As Int32 = 0 To vPageLink.Length - 1
                If j = curPage Then
                    vPageLink(j).Enabled = False
                    vPageLink(j).ForeColor = Drawing.Color.Red
                Else
                    vPageLink(j).Enabled = True
                    vPageLink(j).ForeColor = Drawing.Color.Black
                End If
            Next
        Catch ex As Exception
            Dim s1 As String = ex.ToString
        End Try
    End Sub
    Public Sub linkVersionClick(sender As Object, e As EventArgs)
        Try
            Dim i As Int32
            'Int32.TryParse(sender.text, curVersion)
            Dim o As Object = sender.commandname
            If o IsNot Nothing AndAlso _
            Int32.TryParse(o, i) Then
                curVersion = i
                'populateVersions()
                'populateNumPages()
                'dspPage()
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Sub linkPagesClick(sender As Object, e As EventArgs)
        Try
            Dim i As Int32
            Dim o As Object = sender.commandname
            If o IsNot Nothing AndAlso _
            Int32.TryParse(o, i) Then
                curPage = i
                'Session.Add(sAddVerControls, False)
                'Session.Add(sAddPageControls, False)
                'populateVersions()
                'populateNumPages()
                dspPage()
                For j As Int32 = 0 To vPageLink.Length - 1
                    If j = curPage Then
                        vPageLink(j).Enabled = False
                    Else
                        vPageLink(j).Enabled = True
                    End If
                Next
            End If
            populateNumPages()
        Catch ex As Exception

        End Try
    End Sub
    'Public Sub MenuClick(ByVal sender As Object, ByVal e As MenuEventArgs) Handles Menu1.MenuItemClick
    '    Try
    '        Dim o
    '        o = Request.QueryString("page")
    '        If o IsNot Nothing Then
    '            If Int32.TryParse(CStr(o), curPage) Then
    '                dspPage()
    '            End If
    '        End If
    '    Catch ex As Exception

    '    End Try
    'End Sub

    Private Sub btnLeft_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnLeft.Click
        Try
            curPage -= 1
            populateNumPages()
            dspPage()
        Catch ex As Exception

        End Try
    End Sub
    Private Sub btnRight_Click(ByVal sender As Object, ByVal e As System.EventArgs) Handles btnRight.Click
        Try
            curPage += 1
            populateNumPages()
            dspPage()
        Catch ex As Exception

        End Try
    End Sub
    Sub dspPage()
        Try
            If curPage >= vImg(curVersion).Length OrElse curPage < 0 Then
                curPage = 0
            End If
            Dim i As Int32 = curPage
            img.ImageUrl = String.Format("~/img_tutorialv8_4_{0}/", curVersion) + vImg(curVersion)(i)
            Dim pos As Int32 = InStrRev(vImg(curVersion)(i), ".")
            img.AlternateText = Mid(vImg(curVersion)(i), 1, pos - 1)
            If i = 0 Then
                btnLeft.Enabled = False
                Me.TableLink1.Visible = True
            Else
                btnLeft.Enabled = True
                Me.TableLink1.Visible = True
            End If

            lblCurPage.Text = "Page " + vMn(curVersion)(i)

            If i = vMn(curVersion).Length - 1 Then
                btnRight.Enabled = False
            Else
                btnRight.Enabled = True
            End If

        Catch ex As Exception
            Dim s As String = ex.ToString
        End Try

    End Sub

End Class