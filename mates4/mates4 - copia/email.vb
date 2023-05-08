Imports System.Net.Mail
Public Class email

    Dim attach As System.Net.Mail.Attachment
    Dim Enviado As Int32 = 0
    Dim nomOrig As String

    'Sub init(Request As System.Web.HttpRequest, _
    '         Response As System.Web.HttpResponse)
    '    Dim nVueltas As Int32 = 0
    '    Try
    '        Dim t As DateTime = Now
    '        Dim e1 As String = Now.ToShortDateString + Now.ToShortTimeString
    '        e1 = String.Format("{0:0000}{1:00}{2:00}_{3:00h}{4:00m}{5:00s}", t.Year, t.Month, t.Day, t.Hour, t.Minute, t.Second)
    '        Dim e2 As String = Request.QueryString("nom")
    '        If e2 Is Nothing Then e2 = ""
    '        e1 = "Fax" + e2 + "_" + e1
    '        e1 = System.Text.RegularExpressions.Regex.Replace(e1, "[\(\)\-\: ]", "_")
    '        'e1 = e1.Replace("-", "_")
    '        'e1 = e1.Replace("/", "")
    '        'e1 = e1.Replace(":", "")
    '        'e1 = e1.Replace(" ", "")
    '        nomOrig = e2

    '        Dim ruta As String = "C:\sites\premium2\xrjunque\database\Agentia\faxes\" + e1 + ".tif"
    '        Dim st As IO.Stream = IO.File.Open(ruta, IO.FileMode.Create, IO.FileAccess.Write)
    '        Dim b(CInt(Request.InputStream.Length) - 1) As Byte
    '        Request.InputStream.Read(b, 0, b.Length)
    '        st.Write(b, 0, b.Length)
    '        st.Flush()
    '        st.Close()
    '        Response.Clear()
    '        Response.ClearHeaders()
    '        Response.ClearContent()
    '        Response.ContentType = "plain/text"
    '        Response.Write("GRABADO EN DISCO: " + e1 + ".tif")

    '        'ruta = "E:\new\Agentia Junfor\buscarFaxes\pru"
    '        'Request.InputStream.Seek(0, IO.SeekOrigin.Begin)
    '        attach = New System.Net.Mail.Attachment(ruta) 'Request.InputStream(), "fax" + e1 + ".tif", "image/tiff")
    '        Do While nVueltas < 400
    '            Enviado = 0
    '            send()
    '            send2()
    '            If Enviado = 2 Then Exit Do
    '            System.Threading.Thread.Sleep(100)
    '            nVueltas += 1
    '        Loop
    '        '                                     no cambiar el "¡¡Todo bien!!" porque
    '        '                                   se basa en el texto si fue bien o no:
    '        If Enviado = 2 Then Response.Write(" ¡¡Todo bien!! Emails ENVIADOS.")


    '        ' Response.End()
    '    Catch ex As Exception
    '        Response.Write(ex.ToString())
    '    End Try
    'End Sub

    Public Shared Function SendMailMultipleAttachments(ByVal de As String, _
        ByVal sendTo As String, ByVal Subject As String, _
        ByVal Body As String, _
        Optional ByVal AttachmentFiles() As String = Nothing, _
        Optional ByVal CC As String = "", _
        Optional ByVal BCC As String = "", _
        Optional ByVal SMTPServer As String = "") As Boolean

        'Dim myMessage As System.Web.Mail.MailMessage
        Dim ret As Boolean = False
        Try
            Dim m As New SmtpClient()
            '//eg:localhost, 192.168.0.x, replace with your server name
            m.Host = "62.57.0.65"
            m.Send(de, sendTo, Subject, Body)
            ret = True
        Catch ex As Exception
            Dim s1 As String = ex.ToString
            Dim s2 As String = s1
            'Console.WriteLine("EXCEPTION " + err.Message + vbCrLf)
        End Try
        Return ret
    End Function
    Function send(Optional ByRef ex As Exception = Nothing) As Boolean
        Dim bRet As Boolean = False
        Dim NetMail As New Net.Mail.MailMessage
        Dim MailClient As New Net.Mail.SmtpClient
        Dim TheseCredentials As New Net.NetworkCredential("comments@xrjunque.nom.es", "rosafont")
        Try
            NetMail.From = New Net.Mail.MailAddress("From Calculator of Polynomials<ot@xrjunque.nom.es>")
            NetMail.To.Add(New Net.Mail.MailAddress("To Alfred <info@agentiajunfor.com>"))

            NetMail.IsBodyHtml = True
            NetMail.Subject = "reenvio de fax: " + nomOrig
            NetMail.Body = "This is the body of the message."

            NetMail.Attachments.Add(attach)

            MailClient.Host = "sendmail.brinkster.com"
            MailClient.Port = 25
            MailClient.UseDefaultCredentials = False
            MailClient.Credentials = TheseCredentials
            MailClient.Send(NetMail)
            Enviado += 1
            bRet = True
        Catch EXC As Exception
            'Response.Write(EXC.Message)
            ex = EXC
        Finally
            NetMail = Nothing
            MailClient = Nothing
        End Try
        Return bRet
    End Function
    Public Shared Function send2(subject As String, _
                   body As String, _
                   isHTML As Boolean, _
                   Optional ByRef ex As Exception = Nothing) As Boolean
        Dim bRet As Boolean = False
        Dim NetMail As New Net.Mail.MailMessage
        Dim MailClient As New Net.Mail.SmtpClient
        Dim TheseCredentials As New Net.NetworkCredential("comments@xrjunque.nom.es", "nino20130325")
        Try
            NetMail.From = New Net.Mail.MailAddress("From Calculator of Polynomials<comments@xrjunque.nom.es>")
            NetMail.To.Add(New Net.Mail.MailAddress("To Xavier <xrjunque@hotmail.com>"))

            NetMail.IsBodyHtml = isHTML
            NetMail.Subject = subject ' "reenvio de fax: " + nomOrig
            NetMail.Body = body ' "This is the body of the message."
            'dim mA as new Net.Mail.Attachment(ctype(Request.InputStream(),IO.Stream))

            'NetMail.Attachments.Add(attach)

            MailClient.Host = "sendmail.brinkster.com"
            MailClient.Port = 25
            MailClient.UseDefaultCredentials = False
            MailClient.Credentials = TheseCredentials
            MailClient.Send(NetMail)
            'Enviado += 1
            bRet = True

        Catch EXC As Exception
            'Response.Write(EXC.Message)
            ex = EXC
        Finally
            NetMail = Nothing
            MailClient = Nothing
        End Try
        Return bRet
    End Function

End Class
