Public Class ExprVector
    Public vExpr(-1) As Expression
    Public Sub New()
    End Sub
    Public Sub New(vA As ExprVector)
        Try
            ReDim vExpr(vA.vExpr.Length - 1)
            For i = 0 To vA.vExpr.Length - 1
                vExpr(i) = New Expression(vA.vExpr(i))
            Next
            sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Function FromExprArray(vExpr() As Expression) As ExprVector
        Dim vRet As New ExprVector
        Try
            ReDim vRet.vExpr(vExpr.Length - 1)
            For i = 0 To vExpr.Length - 1
                vRet.vExpr(i) = New Expression(vExpr(i))
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Function
    Public Shared Operator +(vA As ExprVector, vB As ExprVector) As ExprVector
        Dim vRet As ExprVector
        Try
            If vA.vExpr.Length <> vB.vExpr.Length Then
                Throw New Exception(Msg10.Num(107))
            End If
            vRet = New ExprVector(vA)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) += vB.vExpr(i)
                vRet.vExpr(i).Reduce()
            Next
            'vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator -(vA As ExprVector) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector(vA)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i).OpChgSign()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator -(vA As ExprVector, vB As ExprVector) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector(vA)
            vRet += -vB
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator *(vA As ExprVector, vB As ExprVector) As Expression
        Dim vRet As Expression
        Try
            If vA.vExpr.Length <> vB.vExpr.Length Then
                Throw New Exception(Msg10.Num(107))
            End If
            vRet = New Expression(0.0)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                Dim expr As Expression = vA.vExpr(i) * vB.vExpr(i)
                vRet += expr
                vRet.Reduce()
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator *(vA As ExprVector, scalar As Double) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector(vA)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vRet.vExpr(i) * New Expression(scalar)
                vRet.vExpr(i).Reduce()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator *(vA As ExprVector, scalar As Complex) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector(vA)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vRet.vExpr(i) * New Expression(scalar)
                vRet.vExpr(i).Reduce()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator *(vA As ExprVector, scalar As Polynomial) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector(vA)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vRet.vExpr(i) * New Expression(scalar)
                vRet.vExpr(i).Reduce()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator *(vA As ExprVector, scalar As Expression) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector()
            ReDim vRet.vExpr(vA.vExpr.Length - 1)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vA.vExpr(i) * scalar
            Next
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator /(vA As ExprVector, scalar As Double) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector()
            ReDim vRet.vExpr(vA.vExpr.Length - 1)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vA.vExpr(i) / New Expression(scalar)
                vRet.vExpr(i).Reduce()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator /(vA As ExprVector, scalar As Complex) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector()
            ReDim vRet.vExpr(vA.vExpr.Length - 1)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vA.vExpr(i) / New Expression(scalar)
                vRet.vExpr(i).Reduce()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator /(vA As ExprVector, scalar As Polynomial) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector()
            ReDim vRet.vExpr(vA.vExpr.Length - 1)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vA.vExpr(i) / New Expression(scalar)
                vRet.vExpr(i).Reduce()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Shared Operator /(vA As ExprVector, expr As Expression) As ExprVector
        Dim vRet As ExprVector
        Try
            vRet = New ExprVector()
            ReDim vRet.vExpr(vA.vExpr.Length - 1)
            For i As Int32 = 0 To vA.vExpr.Length - 1
                vRet.vExpr(i) = vA.vExpr(i) / expr
                vRet.vExpr(i).Reduce()
            Next
            vRet.sortExprTerms()
        Catch ex As Exception
            Throw
        End Try
        Return vRet
    End Operator
    Public Property Item(index As Int32) As Expression
        Get
            If index >= vExpr.Length Then
                Throw New ArgumentOutOfRangeException
            End If
            Return New Expression(vExpr(index))
        End Get
        Set(value As Expression)
            If index >= vExpr.Length Then
                Throw New ArgumentOutOfRangeException
            End If
            vExpr(index) = New Expression(value)
        End Set
    End Property
    Public ReadOnly Property NextNotZero(iCol As Int32) As Int32
        Get
            Dim notZero As Int32 = -1
            For i As Int32 = iCol To vExpr.Length - 1
                If Not (vExpr(i).IsDouble AndAlso vExpr(i).ToDouble = 0.0) Then
                    notZero = i
                    Exit For
                End If
            Next
            Return notZero
        End Get
    End Property
    Public ReadOnly Property ToArray As Expression()
        Get
            Return vExpr
        End Get
    End Property
    Public Sub sortExprTerms()
        For i As Int32 = 0 To vExpr.Length - 1
            vExpr(i).SortExprTerms()
        Next
    End Sub
    Public Overrides Function ToString() As String
        Dim s As String = ""
        Try
            For j = 0 To vExpr.Length - 1
                s += vExpr(j).ToString
                If j < vExpr.Length - 1 Then s += "; "
            Next
        Catch ex As Exception
        End Try
        Return s
    End Function
End Class
