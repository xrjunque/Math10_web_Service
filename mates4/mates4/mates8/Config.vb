Imports System.Threading

<Serializable()> _
Public Class Config
    ' Configuration for input data:
    Friend ID As Int64 = 0
    Shared vCfg(-1) As Config
    Shared vPID(-1) As Int64
    Shared mtx As New Mutex(False)

    Dim bCaseSensitive1 As Boolean
    Dim bIgnoreCR As Boolean
    Dim bRounding1 As Boolean = False
    Dim bUseEng1 As Boolean = True
    Dim bFractions1 As Boolean = False
    Dim bDetail1 As Boolean = False
    Dim bVarName1Char1 As Boolean = True
    Dim bNumInVarName1 As Boolean = False
    Public bAltMtxOutput As Boolean = False
    Dim oDetail1 As Detall = Nothing
    Dim sImg1 As String = "i"
    Dim ticksStart1 As Long = Now.Ticks
    Dim timeOutms1 As Long = 45000 ' miliseconds (10 seconds)
    Public mathGlobal As MathGlobal8

    ' Config. for output data:
    Dim ToStringBase1 As MathGlobal8.outputBase = _
        MathGlobal8.outputBase.decimal
    Dim nDecimals1 As Int64 = 3

    Public doTimeOut As timeout = timeout.whenTimeIsOver

    Public cur As currentMatch = Nothing
    Public sesID As Int64 = -1
    Public bCancel As Boolean = False
    Public outputFormat As outputMsgFormat = outputMsgFormat.HTML
    Friend posLastOptor As currentMatch.exprSubType2


    Public Sub New()
        MyBase.New()
        mathGlobal = New MathGlobal8(Me)
        initialize()
        ticksStart1 = Now.Ticks
    End Sub
    Public Sub New(startTimeTicks As Long)
        MyBase.New()
        mathGlobal = New MathGlobal8(Me)
        initialize()
        ticksStart1 = startTimeTicks ' MathGlobal8.ticksStart
    End Sub

    Public Sub New(ByVal cfg As Config)
        mtx.WaitOne()
        Try
            With cfg
                bCaseSensitive1 = .bCaseSensitive1
                bIgnoreCR = .bIgnoreCR
                bRounding1 = .bRounding1
                bUseEng1 = .bUseEng1
                bFractions1 = .bFractions1
                bDetail1 = .bDetail1
                bVarName1Char1 = .bVarName1Char1
                bNumInVarName1 = .bNumInVarName1
                oDetail1 = New Detall(cfg.cur)
                sImg1 = .sImg1
                timeOutms1 = .timeOutms1
                ToStringBase1 = .ToStringBase1
                'degrees1 = .degrees1
                nDecimals1 = .nDecimals1
                ticksStart1 = .ticksStart
                doTimeOut = .doTimeOut
                sImg1 = .sImg1
                cur = .cur
                outputFormat = .outputFormat
            End With
            mathGlobal = New MathGlobal8(Me)
            initialize()
        Catch ex As Exception
            mtx.ReleaseMutex()
            Throw ex
        End Try
        mtx.ReleaseMutex()
    End Sub

    Public Shared Property cfg As Config
        Get
            Dim pos As Int64
            Try
                If Not mtx.WaitOne(3000) Then
                    Throw New Exception(msg8.num(13))
                End If
                'SyncLock "cfg"
                Dim ID As Int64 = _
                     System.Threading.Thread.CurrentThread.ManagedThreadId
                pos = Array.IndexOf(vPID, ID)

                If pos = -1 Then
                    For pos = 1 To vCfg.Length - 1
                        If vCfg(pos) Is Nothing Then
                            vCfg(pos) = New Config
                            Exit For
                        ElseIf vCfg(pos).ID = 0 Then
                            Exit For
                        End If
                    Next
                    If pos >= vCfg.Length Then
                        ReDim Preserve vCfg(pos), vPID(pos)
                        vCfg(pos) = New Config
                        vPID(pos) = ID
                    End If
                End If
                'End SyncLock
            Catch ex As Exception
                Dim s1 As String = ex.ToString
                Dim s2 As String = s1
                Throw ex
            Finally
                mtx.ReleaseMutex()
            End Try
            Return vCfg(pos)
        End Get
        Set(value As Config)
            'SyncLock "cfg"
            mtx.WaitOne()
            Try
                Dim ID As Int64 =
                     System.Threading.Thread.CurrentThread.ManagedThreadId
                Dim pos As Int64 = Array.IndexOf(vPID, ID)

                If pos = -1 Then
                    For pos = 1 To vCfg.Length - 1
                        If vCfg(pos) Is Nothing OrElse
                        vCfg(pos).ID = 0 Then
                            Exit For
                        End If
                    Next
                    If pos >= vCfg.Length Then
                        ReDim Preserve vCfg(pos), vPID(pos)
                    End If
                    vCfg(pos) = value
                    vPID(pos) = ID
                End If
                vCfg(pos).ID = ID
                value.ID = ID
            Catch ex As Exception

            End Try
            mtx.ReleaseMutex()
            'End SyncLock

        End Set
    End Property
    Public Sub setCurrentThreadPID()
        vPID(ID) = System.Threading.Thread.CurrentThread.ManagedThreadId
    End Sub
    Public Shared Sub reset(cfg As Config)
        Try
            Dim ID As Int64 =
                     System.Threading.Thread.CurrentThread.ManagedThreadId
            ReDim Preserve vCfg(1), vPID(1)
            vCfg(1) = cfg
            vPID(1) = ID
        Catch ex As Exception

        End Try
    End Sub

    Public Shared Sub cancelID(ID As Int64)
        mtx.WaitOne()
        Try
            Dim i As Int64 = Array.IndexOf(vPID, ID)
            If i > -1 Then vPID(i) = 0
        Catch ex As Exception

        End Try
        mtx.ReleaseMutex()
        Try
            If vPID(vPID.Length - 1) = 0 Then
                ReDim Preserve vPID(vPID.Length - 2), vCfg(vCfg.Length - 2)
            End If
        Catch ex As Exception

        End Try
    End Sub
    Public Sub initialize()
        Try
            mathGlobal.Initialize(Me)
            Me.ticksStart1 = Now.Ticks
            Config.cfg = Me
            'If ID = 0 Then
            '    Dim i As Int64
            '    For i = 1 To vCfg.Length - 1
            '        If vCfg(i) Is Nothing OrElse _
            '        vCfg(i).ID = 0 Then
            '            Exit For
            '        End If
            '    Next
            '    If i < vCfg.Length Then
            '        vCfg(i) = Me
            '        vCfg(i).ID = i
            '    Else
            '        GID += 1
            '        ID = GID
            '        ReDim Preserve vCfg(ID), vPID(ID)
            '        vCfg(ID) = Me
            '        setCurrentThreadPID()
            '    End If
            'End If
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub resetStartTime()
        ticksStart1 = Now.Ticks
    End Sub
    Public Property sImg As String
        Get
            Return sImg1
        End Get
        Set(ByVal value As String)
            If value = "j" Then
                sImg1 = value
            Else
                sImg1 = "i"
            End If
            initialize()
        End Set
    End Property
    Public Property bVarName1Char As Boolean
        Get
            Return Me.bVarName1Char1
        End Get
        Set(ByVal value As Boolean)
            If Me.bVarName1Char1 <> value Then
                Me.bVarName1Char1 = value
                initialize()
            End If
        End Set
    End Property
    Public Property bNumsInVarName As Boolean
        Get
            Return Me.bNumInVarName1
        End Get
        Set(ByVal value As Boolean)
            If Me.bNumInVarName1 <> value Then
                Me.bNumInVarName1 = value
                initialize()
            End If
        End Set
    End Property
    Public Property bIgnoreSpaces As Boolean
        Get
            Return bIgnoreCR
        End Get
        Set(ByVal value As Boolean)
            bIgnoreCR = value
        End Set
    End Property
    Public Property bCaseSensitive As Boolean
        Get
            Return bCaseSensitive1
        End Get
        Set(value As Boolean)
            bCaseSensitive1 = value
        End Set
    End Property
    Public Property bRounding As Boolean
        Get
            Return bRounding1
        End Get
        Set(value As Boolean)
            bRounding1 = value
        End Set
    End Property
    Public Property bDetail As Boolean
        Get
            Return bDetail1
        End Get
        Set(value As Boolean)
            bDetail1 = value
        End Set
    End Property
    Public Property bEngNotation As Boolean
        Get
            Return bUseEng1
        End Get
        Set(value As Boolean)
            bUseEng1 = value
        End Set
    End Property
    Public Property bFractions As Boolean
        Get
            Return bFractions1
        End Get
        Set(value As Boolean)
            bFractions1 = value
        End Set
    End Property
    'Public Property degreesType As MathGlobal8.degreesType
    '    Get
    '        Return degrees1
    '    End Get
    '    Set(value As MathGlobal8.degreesType)
    '        degrees1 = value
    '    End Set
    'End Property
    Public Property Base As MathGlobal8.outputBase
        Get
            Return Me.ToStringBase1
        End Get
        Set(value As MathGlobal8.outputBase)
            Me.ToStringBase1 = value
        End Set
    End Property
    Public Property nDecimals As Int64
        Get
            Return Me.nDecimals1
        End Get
        Set(value As Int64)
            Me.nDecimals1 = value
        End Set
    End Property
    Public Property oDetail As Detall
        Get
            If oDetail1 Is Nothing AndAlso _
            Me.cur IsNot Nothing Then
                oDetail1 = New Detall(Me.cur)
            End If
            Return oDetail1
        End Get
        Set(value As Detall)
            oDetail1 = value
        End Set
    End Property
    Friend Property ticksStart As Long
        Get
            If ticksStart1 = 0 Then
                ticksStart1 = Now.Ticks
            End If
            Return ticksStart1
        End Get
        Set(value As Long)

            If value > 0 AndAlso value <> MathGlobal8.ticksStart Then
                MathGlobal8.lnCount = 0
            End If
            If value <= 0 Then value = Now.Ticks
            MathGlobal8.ticksStart = value
            Dim dtNow As New DateTime(value)
            'Trace.WriteLine("start timeout time=" + dtNow.ToShortTimeString + _
            '                " " + dtNow.Second.ToString + _
            '                " " + dtNow.Millisecond.ToString)
            ticksStart1 = value
        End Set
    End Property
    Public Property timeOutms As Long
        Get
            Return timeOutms1
        End Get
        Set(value As Long)

            MathGlobal8.timeOutms = value
            timeOutms1 = value
        End Set
    End Property
    Public Function isTimeoutFraction(ByRef tantoPorUno As Double) As Boolean

        'Return False ' uncomment for debugging purposes

        Dim bRet As Boolean = True
        If ID Then

            System.Threading.Thread.Sleep(0)
            Dim ln As Long = Now.Ticks
            Dim tpu As Double = (ln - ticksStart) / (timeOutms * 10 ^ 4)
            If doTimeOut = timeout.never Then
                bRet = False
            ElseIf doTimeOut = timeout.always Then
            ElseIf tpu < tantoPorUno Then
                bRet = False
            Else
                Dim dtNow As New DateTime(ln)
                Dim dttmOut As New DateTime(ticksStart + timeOutms)
                ticksStart = Now.Ticks + timeOutms * 10 ^ 4
            End If
            tantoPorUno = tpu
        End If
        'If bRet Then
        'Throw New Exception(msg8.num(81))
        'End If
        Return bRet
    End Function
    Public Function isTimeout(Optional throwErr As Boolean = True) As Boolean
        'Return False ' uncomment for debugging purposes

        System.Threading.Thread.Sleep(0)
        If ID = 0 Then
            Thread.CurrentThread.Abort()
        End If
        Dim i As Int64 = Array.IndexOf(vPID, ID)
        If i = -1 Then
            cancelID(ID)
            Thread.CurrentThread.Abort()
        End If
        If doTimeOut = timeout.never Then
            Return False
        ElseIf doTimeOut = timeout.always Then
            If throwErr Then
                Throw New Exception(msg8.num(81))
            End If
            Return True
        End If
        Dim ln As Long = Now.Ticks
        If ticksStart = 0 OrElse timeOutms = 0 Then
            Return False
        End If
        If ln > ticksStart + timeOutms * 10 ^ 4 Then
            Return True
        End If
        Return False
        'Dim dtNow As New DateTime(ln)

        ''Trace.WriteLine("timeout stop at=" + dtNow.ToShortTimeString + _
        ''                " " + dtNow.Second.ToString + _
        ''                " " + dtNow.Millisecond.ToString)
        ''Dim dttmOut As New DateTime(ticksStart + timeOutms)
        ''ticksStart = Now.Ticks + timeOutms * 10 ^ 4
        'If throwErr Then
        '    Throw New Exception(msg8.num(81))
        'End If
        'Return True
    End Function

End Class
Public Enum outputMsgFormat
    plainText
    RichTextFormat
    HTML
End Enum
Public Enum timeout
    always = 0
    whenTimeIsOver = 1
    never = 2
End Enum