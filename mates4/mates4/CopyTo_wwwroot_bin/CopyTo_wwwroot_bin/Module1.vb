Imports System.Windows.Forms

Module Module1
    'Dim IsEven As Boolean
    'Dim srcA As String = "C:\Users\xrjun\OneDrive\Documentos\Visual Studio 2010\Projects\mates4\mates4\mates4\bin\copybin\mates4.dll"
    'Dim srcB As String = "C:\Users\xrjun\OneDrive\Documentos\Visual Studio 2010\Projects\mates4\mates4\mates4\bin\copybin2\mates4.dll"
    'Dim dst As String = "C:\inetpub\wwwroot\bin\mates4.dll"
    Dim i As Int32
    Sub Main()
        Dim vs() As String = New String() {
            "https://xrjunque.nom.es",
            "https://xrjunque.nom.es/home.aspx",
            "https://xrjunque.nom.es/mobile.aspx",
            "https://xrjunque.nom.es/polycalc.aspx",
            "https://xrjunque.nom.es/swdownload.aspx",
            "https://www.xrjunque.nom.es/MathML_To_String.aspx",
            "https://www.google.es",
            "https://www.lavanguardia.com"}
        Dim rnd As New Random

        If False Then
            Do
                Try
                    Dim j As Int32 = i Mod vs.Length
                    Dim sWeb As String = vs(j)
                    Dim wc As New Net.WebClient
                    Dim sErr As String = ""
                    Try
                        Using s As IO.Stream = wc.OpenRead(sWeb)
                            Using sr As New IO.StreamReader(s)
                                Dim s1 As String = sr.ReadToEnd
                                Console.WriteLine(Left(s1, 10))
                            End Using
                        End Using
                    Catch ex2 As Exception
                        sErr = ex2.ToString
                    End Try
                    i += 1
                    Console.WriteLine(i.ToString + " " + sWeb + " " + sErr)
                    Dim t As Int32 = rnd.Next(3, 5)
                    System.Threading.Thread.Sleep(t * 1000)
                Catch ex As Exception

                End Try
            Loop
        Else
            Do
                For j As Int32 = 0 To vs.Length - 1
                    Try
                        Dim p As New Process()
                        p.Start("C:\Program Files (x86)\Microsoft\Edge\Application\msedge.exe", vs(j))
                        Console.WriteLine(vs(j))
                        Threading.Thread.Sleep(5000)
                        SendKeys.SendWait("%{F4}")
                        'Dim p1 As Process() = Process.GetProcessesByName("msedge")
                        'For i = 0 To p1.Count - 1
                        '    p1(i).Kill()
                        'Next
                        'Dim TskKill_Edge As New ProcessStartInfo("Taskkill.exe")
                        'TskKill_Edge.Arguments = "/F /IM MSEdge.exe /T"
                        'Process.Start(TskKill_Edge)
                        If j Mod 10 = 0 Then
                            Process.Start("c:\windows\system32\inetsrv\appcmd.exe", "recycle apppool ""Default Web Site""")
                        End If
                        Threading.Thread.Sleep(2500)
                    Catch ex As Exception
                    End Try

                Next
            Loop
        End If
    End Sub
    Sub cmd(command As String, arguments As String, permanent As Boolean)
        Dim p As Process = New Process()
        Dim pi As ProcessStartInfo = New ProcessStartInfo()
        pi.Arguments = " " + If(permanent = True, "/K", "/C") + " " + command + " " + arguments
        pi.FileName = "cmd.exe"
        p.StartInfo = pi
        p.Start()
    End Sub
End Module
