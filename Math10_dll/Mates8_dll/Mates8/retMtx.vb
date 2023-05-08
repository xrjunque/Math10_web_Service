Imports System.Text

<Serializable()> _
Public Class retMtx
    Public Sub New()
        MyBase.New()
    End Sub

    Public exprMtx As ExprMatrix ' New ExprMatrix(1, 1)
    'Public DerMtx As ExprMatrix ' New ExprMatrix(1, 1)
    'Public sgn As Int64 = 1
    Friend vars As VarsAndFns
    Dim iRow, iCol As Int64
    Dim iRowOld, iColOld As Int64
    Dim parser As exprParser
    Dim cfg1 As Config = Config.cfg
    Friend diffType As diffResponse = diffResponse.resto
    Public soe As SystemOfEquations
    Public Sub New(ByVal rMtx As retMtx)
        With rMtx
            If .exprMtx IsNot Nothing Then
                Me.exprMtx = New ExprMatrix(.cfg, 1, 1)
                Me.exprMtx = New ExprMatrix(.exprMtx)
                If .parser IsNot Nothing Then
                    Me.parser = .parser
                End If
            End If
            'If .DerMtx IsNot Nothing Then
            '    Me.DerMtx = New ExprMatrix(.cfg, 1, 1)
            '    Me.DerMtx = New ExprMatrix(.DerMtx)
            'End If
            iCol = .iCol : iRow = .iRow
            If .vars IsNot Nothing Then
                Me.vars = New VarsAndFns(.vars)
            End If
        End With
    End Sub
    Public Sub New(ByRef parser As exprParser)
        Me.exprMtx = New ExprMatrix(parser.cfg, 1, 1)
        'Me.DerMtx = New ExprMatrix(parser.cur.cfg, 1, 1)
        Me.parser = parser
    End Sub
    Public Sub New(ByRef parser As exprParser, ByVal exprMtx As ExprMatrix)
        Me.exprMtx = New ExprMatrix(parser.cfg, 1, 1)
        'Me.DerMtx = New ExprMatrix(parser.cur.cfg, 1, 1)
        Me.parser = parser
        Me.exprMtx = New ExprMatrix(exprMtx)
    End Sub
    Public Shared Function parseRetMtx(ByVal rtMtx As retMtx) As retMtx
        'Return MathGlobal8.CloneObject(ret)
        Dim ret(0) As retMtx
        Try
            Dim arr() As retMtx = {rtMtx}
            Array.Copy(arr, ret, 1)
        Catch ex As Exception
            Throw ex
        End Try
        Return ret(0)
    End Function
    Public ReadOnly Property getParser() As exprParser
        Get
            Return parser
        End Get
    End Property
    Public ReadOnly Property isMatrix As Boolean
        Get
            Return (rows + cols > 2)
        End Get
    End Property
    Public Property cfg As Config
        Get
            Return cfg1
        End Get
        Set(value As Config)
            cfg1 = value
        End Set
    End Property
    Public Property curExpr As Expression
        Get
            If iCol >= cols Then iCol = cols - 1
            If iRow >= rows Then iRow = rows - 1
            Return Me.exprMtx.getExpr(iRow, iCol)
        End Get
        Set(ByVal value As Expression)
            Me.exprMtx.getExpr(iRow, iCol) = value
        End Set
    End Property
    Public Sub setRetVal(ByVal ret As retVal)
        exprMtx.getExpr(iRow, iCol) = ret.expr
    End Sub
    Public Sub saveColRow()
        iColOld = iCol
        iRowOld = iRow
    End Sub
    Public Sub restoreColRow()
        iCol = iColOld
        iRow = iRowOld
        Me.exprMtx.setExprTo(cfg, iRow + 1, iCol + 1)
    End Sub
    Public Sub incrRow()
        iRow += 1
        iCol = 0
        exprMtx.Add(iRow, iCol, 0.0)
    End Sub
    Public Sub incrCol()
        iCol += 1
        exprMtx.Add(iRow, iCol, 0.0)
    End Sub
    Public ReadOnly Property rows As Int64
        Get
            Return Me.exprMtx.Rows
        End Get
    End Property
    Public ReadOnly Property cols As Int64
        Get
            Return Me.exprMtx.Cols
        End Get
    End Property


    Public Overrides Function ToString() As String
        Return toStringRetVal(Config.cfg)
    End Function
    Public ReadOnly Property toStringRetVal(cfg As Config) As String
        Get
            If diffType = diffResponse.varValue Then
                Dim sb As New StringBuilder
                For i = 0 To vars.getNamesList.Length - 1
                    sb.Append(vars.getNamesList(i) + " = ")
                    sb.Append(vars.getValueByID(i).ToStrExprMtx(cfg) + vbCrLf)
                Next
                Return sb.ToString
            ElseIf diffType = diffResponse.implicit Then
                Dim sb As New StringBuilder
                For i = 0 To vars.getNamesList.Length - 1 Step 2
                    sb.Append(vars.getValueByID(i).ToStrExprMtx(cfg) + " = ")
                    sb.Append(vars.getValueByID(i + 1).ToStrExprMtx(cfg) + vbCrLf)
                Next
                Return sb.ToString
            End If
            Return exprMtx.ToStringExprMtx(cfg)
        End Get
    End Property

    Public Enum diffResponse As Int64
        resto = 0
        varValue = 1
        implicit = 2
        polynomial = 4
    End Enum
End Class
