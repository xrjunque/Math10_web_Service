
Public Class ExprMatrix
    Dim vExpr(-1)() As Expression
    Public Sub New()
    End Sub
    Public Sub New(db As Double)
        ReDim vExpr(0), vExpr(0)(0)
        vExpr(0)(0) = New Expression(db)
    End Sub
    Public Sub New(cjo As Complex)
        ReDim vExpr(0), vExpr(0)(0)
        vExpr(0)(0) = New Expression(cjo)
    End Sub
    Public Sub New(vcjo() As Complex)
        ReDim vExpr(0), vExpr(0)(vcjo.Length - 1)
        For i As Int32 = 0 To vcjo.Length - 1
            vExpr(0)(i) = New Expression(vcjo(i))
        Next
    End Sub
    Public Sub New(poly As Polynomial)
        poly.Reduce()
        ReDim vExpr(0), vExpr(0)(0)
        vExpr(0)(0) = New Expression(poly)
    End Sub
    Public Sub New(nRows As Int32, nCols As Int32)
        Try
            ReDim vExpr(nRows - 1)
            For i As Int32 = 0 To vExpr.Length - 1
                ReDim vExpr(i)(nCols - 1)
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub New(eMa As ExprMatrix)
        Try
            ReDim vExpr(eMa.vExpr.Length - 1)
            For i As Int32 = 0 To eMa.vExpr.Length - 1
                ReDim Preserve vExpr(i)(eMa.vExpr(i).Length - 1)
                For j As Int32 = 0 To eMa.vExpr(i).Length - 1
                    vExpr(i)(j) = eMa.vExpr(i)(j)
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub New(arExpr() As Expression)
        ReDim vExpr(0), vExpr(0)(arExpr.Length - 1)
        Try
            For j As Int32 = 0 To arExpr.Length - 1
                vExpr(0)(j) = New Expression(arExpr(j))
            Next

        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub New(expr As Expression)
        ReDim vExpr(0), vExpr(0)(0)
        vExpr(0)(0) = expr
    End Sub
    Public Sub AddItemToRow(Expr As Expression, row As Int32, column As Int32)
        Try
            If row >= vExpr.Length Then
                ReDim Preserve vExpr(row)
            End If
            ReDim Preserve vExpr(row)(column)
            vExpr(row)(column) = Expr
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub AddRow(Expr() As Expression)
        Try
            Dim n As Int32 = vExpr.Length
            ReDim Preserve vExpr(n), vExpr(n)(Expr.Length - 1)
            For j As Int32 = 0 To Expr.Length - 1
                vExpr(n)(j) = Expr(j)
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub RedimRowColumn(row As Int32, col As Int32)
        ReDim Preserve vExpr(row), vExpr(row)(col)
    End Sub
    Public Sub AddColumn(column() As Expression)
        Try
            For i = 0 To column.Length - 1
                If vExpr.Length <= i Then
                    ReDim Preserve vExpr(i), vExpr(i)(-1)
                End If
                Dim n As Int32 = vExpr(i).Length
                ReDim Preserve vExpr(i)(n)
                vExpr(i)(n) = column(i)
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Property Item(ByVal row As Int32, ByVal col As Int32) As Expression
        Get
            If row < Me.vExpr.Length AndAlso col < Me.vExpr(row).Length Then
                Return vExpr(row)(col)
            Else
                Throw New ArgumentOutOfRangeException
            End If
        End Get
        Set(value As Expression)
            If row >= 0 AndAlso row < vExpr.Length Then
                vExpr(row)(col) = value
            Else
                Throw New ArgumentOutOfRangeException
            End If
        End Set
    End Property
    Public ReadOnly Property getItems As Expression()()
        Get
            Return vExpr
        End Get
    End Property
    Public ReadOnly Property Rows As Int32
        Get
            Return vExpr.Length
        End Get
    End Property
    Public ReadOnly Property HasRowsOrCols As Boolean
        Get
            Return (Rows > 1 OrElse vExpr(0).Length > 1)
        End Get
    End Property

    Public Function IsExpression() As Boolean
        Return vExpr.Length = 1 AndAlso vExpr(0).Length = 1
    End Function
    Public Function IsPolynomial() As Boolean
        Return IsExpression() AndAlso vExpr(0)(0).IsPolynomial
    End Function
    Public Function IsPolynomialMatrix(ByRef vars() As String) As Boolean
        Try
            Dim lstVars As New List(Of String)
            For i As Int32 = 0 To Rows - 1
                For j As Int32 = 0 To Columns(i) - 1
                    If Not Me.vExpr(i)(j).IsPolynomial Then
                        Return False
                    End If
                    Dim vvars() As String = Me.vExpr(i)(j).ToPolynomial.GetVars
                    For k = 0 To vvars.Length - 1
                        If Not lstVars.Contains(vvars(k)) Then
                            lstVars.Add(vvars(k))
                        End If
                    Next
                Next
            Next
            vars = lstVars.ToArray
        Catch ex As Exception
            Throw
        End Try
        Return True
    End Function
    Public Function IsMatrixOfLinearEqs() As Boolean
        Try
            For i As Int32 = 0 To Rows - 1
                For j As Int32 = 0 To Columns(i) - 1
                    Dim expr As Expression = vExpr(i)(j)
                    For Each t As ExprTerm In expr.t
                        Dim d As Int32 = ExprTerm.Degree(t)
                        If d > 1 Then Return False
                        If t.f.Count > 1 Then Return False
                        If t.f.Count = 1 AndAlso t.f(0).args IsNot Nothing Then
                            Return False
                        End If
                    Next
                    If expr.resto IsNot Nothing Then
                        For Each t As ExprTerm In expr.resto
                            Dim d As Int32 = ExprTerm.Degree(t)
                            If d > 1 Then Return False
                            If t.f.Count > 1 Then Return False
                            If t.f.Count = 1 AndAlso t.f(0).args IsNot Nothing Then
                                Return False
                            End If
                        Next
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return True
    End Function
    Public Sub GetAugmentedMatrixOutFromVars(vVars() As String, ByRef augmentedMtx As ExprMatrix)
        Dim eM As ExprMatrix
        Try
            eM = ExprMatrix.Zero(vVars.Length, vVars.Length + 1)
            For i As Int32 = 0 To Rows - 1
                For j As Int32 = 0 To Columns(i) - 1
                    Dim expr As Expression = vExpr(i)(j)
                    For Each t As ExprTerm In expr.t
                        Dim d As Int32 = ExprTerm.Degree(t)
                        If d = 0 Then
                            eM.Item(i, vVars.Length) -= New Expression(t.Cf)
                        ElseIf t.f(0).args Is Nothing OrElse t.f(0).args.Count = 0 Then
                            Dim pos As Int32 = Array.IndexOf(vVars, t.f(0).var)
                            eM.Item(i, pos) += New Expression(t.Cf)
                        End If
                    Next
                    If expr.resto IsNot Nothing Then
                        Dim cfDiv As Complex = expr.divisor(0).Cf
                        For Each t As ExprTerm In expr.resto
                            Dim d As Int32 = ExprTerm.Degree(t)
                            If d = 0 Then
                                eM.Item(i, vVars.Length) -= New Expression(t.Cf / cfDiv)
                            ElseIf t.f(0).args Is Nothing OrElse t.f(0).args.Count = 0 Then
                                Dim pos As Int32 = Array.IndexOf(vVars, t.f(0).var)
                                eM.Item(i, pos) += New Expression(t.Cf / cfDiv)
                            End If
                        Next
                    End If
                Next
            Next
            augmentedMtx = eM
        Catch ex As Exception
            Throw
        End Try

    End Sub
    Public Function GetVars() As String()
        Dim lstVars As New List(Of String)
        Try
            For i As Int32 = 0 To Rows - 1
                For j As Int32 = 0 To Columns(i) - 1
                    Dim expr As Expression = vExpr(i)(j)
                    For Each t As ExprTerm In expr.t
                        Dim vvars() As String = t.GetVars
                        For Each s As String In vvars
                            If Not lstVars.Contains(s) Then
                                lstVars.Add(s)
                            End If
                        Next
                    Next
                    If expr.resto IsNot Nothing Then
                        For Each t As ExprTerm In expr.resto
                            Dim vvars() As String = t.GetVars
                            For Each s As String In vvars
                                If Not lstVars.Contains(s) Then
                                    lstVars.Add(s)
                                End If
                            Next
                        Next
                        For Each t As ExprTerm In expr.divisor
                            Dim vvars() As String = t.GetVars
                            For Each s As String In vvars
                                If Not lstVars.Contains(s) Then
                                    lstVars.Add(s)
                                End If
                            Next
                        Next
                    End If
                Next
            Next
            lstVars.Sort()
            For i = lstVars.Count - 1 To 1 Step -1
                If lstVars(i) = lstVars(i - 1) Then
                    lstVars.RemoveAt(i)
                End If
            Next

        Catch ex As Exception
            Throw
        End Try
        Return lstVars.ToArray
    End Function
    Public Function IsComplex() As Boolean
        Return IsExpression() AndAlso vExpr(0)(0).IsComplex
    End Function
    Public Function IsDouble() As Boolean
        Return IsExpression() AndAlso vExpr(0)(0).IsDouble
    End Function
    Public Function ToPolynomial() As Polynomial
        If IsPolynomial() Then
            Return vExpr(0)(0).ToPolynomial
        End If
        Return Nothing
    End Function
    Public Function ToComplex() As Complex
        If IsComplex() Then
            Return vExpr(0)(0).ToComplex
        End If
        Return Nothing
    End Function
    Public Function ToDouble() As Double
        If IsDouble() Then
            Return vExpr(0)(0).ToDouble
        End If
        Return Nothing
    End Function
    Public Property Row(iRow As Int32) As Expression()
        Get
            Dim vRet(vExpr(iRow).Length - 1) As Expression
            Try
                For i As Int32 = 0 To vRet.Length - 1
                    vRet(i) = New Expression(vExpr(iRow)(i))
                Next
            Catch ex As Exception
                Throw
            End Try
            Return vRet
        End Get
        Set(value As Expression())
            Try
                For i As Int32 = 0 To value.Length - 1
                    vExpr(iRow)(i) = value(i)
                Next
            Catch ex As Exception
                Throw
            End Try
        End Set
    End Property
    Public Property Column(col As Int32) As Expression()
        Get
            Dim vRet(vExpr.Length - 1) As Expression
            Try
                For i As Int32 = 0 To vExpr.Length - 1
                    vRet(i) = New Expression(vExpr(i)(col))
                Next
            Catch ex As Exception
                Throw
            End Try
            Return vRet
        End Get
        Set(value As Expression())
            Try
                If value.Length <> vExpr.Length Then
                    Throw New Exception(Msg10.Num(109))
                End If
                For i = 0 To value.Length - 1
                    vExpr(i)(col) = value(i)
                Next
            Catch ex As Exception
                Throw
            End Try
        End Set
    End Property
    Public Shared Sub SwapRows(ByRef eMtx As ExprMatrix, rowA As Int32, rowB As Int32)
        Dim eMc As New ExprMatrix
        Try
            If rowA = rowB OrElse rowA >= eMtx.Rows OrElse rowB >= eMtx.Rows Then
                Throw New Exception(Msg10.Num(109))
            End If
            Dim vA As ExprVector = ExprVector.FromExprArray(eMtx.Row(rowA))
            Dim vB As ExprVector = ExprVector.FromExprArray(eMtx.Row(rowB))
            For i = 0 To eMtx.Rows - 1
                If i <> rowA AndAlso i <> rowB Then
                    eMc.AddRow(eMtx.Row(i))
                ElseIf i = rowA Then
                    eMc.AddRow(vB.ToArray)
                Else
                    eMc.AddRow(vA.ToArray)
                End If
            Next
            ReDim eMtx.vExpr(-1)
            For i = 0 To eMc.Rows - 1
                eMtx.AddRow(eMc.Row(i))
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Shared Sub SwapColumns(ByRef eMtx As ExprMatrix, colA As Int32, colB As Int32, vVars() As String)
        Dim eMc As New ExprMatrix
        Try
            If colA = colB Then
                Throw New Exception(Msg10.Num(109))
            End If
            Dim vA As ExprVector = ExprVector.FromExprArray(eMtx.Column(colA))
            Dim vB As ExprVector = ExprVector.FromExprArray(eMtx.Column(colB))
            Dim sVarA As String
            If vVars IsNot Nothing Then
                sVarA = vVars(colA)
                vVars(colA) = vVars(colB)
                vVars(colB) = sVarA
            End If
            For i = 0 To eMtx.Columns(0) - 1
                If i <> colA AndAlso i <> colB Then
                    eMc.AddColumn(eMtx.Column(i))
                ElseIf i = colA Then
                    eMc.AddColumn(vB.ToArray)
                Else
                    eMc.AddColumn(vA.ToArray)
                End If
            Next
            ReDim eMtx.vExpr(-1)
            For i = 0 To eMc.Rows - 1
                eMtx.AddRow(eMc.Row(i))
            Next
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Function GetColumnPivot(ByRef iRow As Int32, ByRef jCol As Int32, IsAugmentedMatrix As Boolean, ByRef vVars() As String, MtxToInvert As ExprMatrix) As Expression
        Dim max As New Expression(vExpr(iRow)(jCol))
        Dim iMax As Int32 = iRow
        Dim jMax As Int32 = jCol
        Try
            Dim i, j As Int32
            For i = iRow To Rows - 1
                j = jCol 'For j = jCol To Me.vExpr(i).Length - 1 - IIf(IsAugmentedMatrix, 1, 0)
                If (max.IsComplex AndAlso vExpr(i)(j).ToComplex.opNorm > max.ToComplex.opNorm) Then
                    iMax = i : jMax = j
                    max = vExpr(i)(j)
                End If
                'Next
            Next
            If max.IsDouble AndAlso max.ToDouble = 0.0 Then
                For i = iRow To Rows - 1
                    For j = jCol To Me.vExpr(i).Length - 1 - IIf(IsAugmentedMatrix, 1, 0)
                        If Not vExpr(i)(j).IsComplex OrElse
                         (max.IsComplex AndAlso vExpr(i)(j).ToComplex.opNorm > max.ToComplex.opNorm) Then
                            iMax = i : jMax = j
                            max = vExpr(i)(j)
                        End If
                    Next
                Next
            End If
            If iRow <> iMax Then
                SwapRows(Me, iRow, iMax)
                If MtxToInvert IsNot Nothing Then
                    SwapRows(MtxToInvert, iRow, iMax)
                End If
                iRow = iMax
            End If
            'If jCol <> jMax Then
            '    SwapColumns(Me, jCol, jMax, vVars)
            '    jCol = jMax
            'End If
        Catch ex As Exception
            Throw
        End Try
        Return max
    End Function
    Public ReadOnly Property Columns(row As Int32) As Int32
        Get
            If row >= 0 AndAlso row < vExpr.Length Then
                Return vExpr(row).Length
            Else
                Throw New ArgumentOutOfRangeException
            End If
        End Get
    End Property
    Public Shared Operator +(eMa As ExprMatrix, eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix
        Try
            If eMa.Rows <> eMb.Rows Then
                Throw New Exception(Msg10.Num(109))
            End If
            eMc = New ExprMatrix(eMa)
            For i As Int32 = 0 To eMc.Rows - 1
                For j As Int32 = 0 To eMc.vExpr(i).Length - 1
                    eMc.vExpr(i)(j) += eMb.vExpr(i)(j)
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator
    Public Shared Operator -(eMa As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix
        Try
            eMc = New ExprMatrix(eMa)
            For i As Int32 = 0 To eMc.Rows - 1
                For j As Int32 = 0 To eMc.vExpr(i).Length - 1
                    eMc.vExpr(i)(j).OpChgSign()
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator
    Public Shared Operator -(eMa As ExprMatrix, eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix
        Try
            eMc = New ExprMatrix(eMa)
            eMc = eMc + (-eMb)
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator

    Public Shared Operator *(eMa As ExprMatrix, eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix
        Try
            eMc = New ExprMatrix()
            ReDim eMc.vExpr(eMa.Rows - 1)
            For i As Int32 = 0 To eMa.Rows - 1
                Dim rowA As ExprVector = ExprVector.FromExprArray(eMa.Row(i))
                ReDim eMc.vExpr(i)(eMb.vExpr(i).Length - 1)
                For j As Int32 = 0 To eMb.vExpr(i).Length - 1
                    Dim colB As ExprVector = ExprVector.FromExprArray(eMb.Column(j))
                    eMc.vExpr(i)(j) = rowA * colB
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator
    Public Shared Operator *(eMa As ExprMatrix, cjo As Complex) As ExprMatrix
        Dim eMc As ExprMatrix
        Try
            eMc = New ExprMatrix()
            ReDim eMc.vExpr(eMa.Rows - 1)
            For i As Int32 = 0 To eMa.Rows - 1
                Dim rowA As ExprVector = ExprVector.FromExprArray(eMa.Row(i))
                ReDim eMc.vExpr(i)(eMa.vExpr(i).Length - 1)
                For j As Int32 = 0 To eMa.vExpr(i).Length - 1
                    eMc.vExpr(i)(j) = eMa.vExpr(i)(j) * New Expression(cjo)
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator
    Public Shared Operator *(eMa As ExprMatrix, expr As Expression) As ExprMatrix
        Dim eMc As ExprMatrix
        Try
            Dim cjo As Complex = Nothing
            If expr.IsComplex Then
                cjo = expr.ToComplex
            End If
            expr.Reduce()
            eMc = New ExprMatrix()
            ReDim eMc.vExpr(eMa.Rows - 1)
            For i As Int32 = 0 To eMa.Rows - 1
                Dim rowA As ExprVector = ExprVector.FromExprArray(eMa.Row(i))
                ReDim eMc.vExpr(i)(eMa.vExpr(i).Length - 1)
                For j As Int32 = 0 To eMa.vExpr(i).Length - 1
                    If eMa.vExpr(i)(j).IsComplex AndAlso cjo IsNot Nothing Then
                        eMc.vExpr(i)(j) = New Expression(eMa.vExpr(i)(j).ToComplex * cjo)
                    Else
                        eMc.vExpr(i)(j) = eMa.vExpr(i)(j) * expr
                    End If
                    eMc.vExpr(i)(j).Reduce()
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator /(ByVal eMa As ExprMatrix, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            If eMa.Rows * eMa.Columns(0) + eMb.Rows * eMb.Columns(0) = 2 Then
                eMc = New ExprMatrix(eMa.Item(0, 0) / eMb.Item(0, 0))
            Else
                eMc = eMa * (eMb ^ New ExprMatrix(-1))
            End If
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator

    Public Overloads Shared Operator ^(ByVal eMa As ExprMatrix, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            Dim sDetail As String = ""
            If G10.detail Then
                sDetail = eMa.ToString +
                        " ^ " + eMb.ToString
            End If
            If eMa.Rows * eMa.Columns(0) + eMb.Rows * eMb.Columns(0) = 2 Then
                eMc = New ExprMatrix(eMa.vExpr(0)(0) ^ eMb.vExpr(0)(0))
            ElseIf eMb.IsDouble Then
                Dim db As Double = eMb.ToDouble
                If True Then
                    Dim absDb As Double = Math.Abs(db)
                    If db < 0 Then
                        Dim InvM As ExprMatrix = Nothing
                        Dim vvars(-1) As String
                        ExprMatrix.GaussianElimination(eMa, False, vvars, True, InvM)
                        eMc = InvM
                    ElseIf db = 0 Then
                        eMc = New ExprMatrix(eMa)
                        For i As Int64 = 0 To eMc.Rows - 1
                            For j As Int64 = 0 To eMc.Columns(0) - 1
                                If i = j Then
                                    eMc.Item(i, j) = New Expression(0.0)
                                Else
                                    eMc.Item(i, j) = New Expression(1.0)
                                End If
                            Next
                        Next
                        Exit Try
                    Else
                        eMc = New ExprMatrix(eMa)
                    End If

                    For i As Int64 = 2 To db
                        eMc *= eMa
                    Next
                End If
            Else
                eMc = New ExprMatrix(eMa.Rows, eMb.Columns(0))
                For i As Int64 = 0 To eMa.Rows - 1
                    For j As Int64 = 0 To eMb.Columns(0) - 1
                        eMc.Item(i, j) = New Expression(0.0)
                        For k As Int64 = 0 To eMb.Rows - 1
                            eMc.Item(i, j) += (eMa.Item(i, k) * eMb.Item(k, j))
                        Next
                    Next
                Next
            End If
            If G10.detail Then
                G10.sDetail += sDetail + " = " +
                    eMc.ToString
            End If
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Operator

    Public Function opExponential() As ExprMatrix
        Dim rMtx As ExprMatrix = Nothing
        Try
            rMtx = New ExprMatrix(Me.Rows, Me.Columns(0))

            Dim Identity As New ExprMatrix(Me.Rows, Me.Columns(0))
            Dim i, j As Int64
            For i = 0 To Identity.Rows - 1
                For j = 0 To Identity.Columns(i) - 1
                    rMtx.Item(i, j) = New Expression(0.0)
                    If i <> j Then
                        Identity.Item(i, j) = New Expression(0.0)
                    Else
                        Identity.Item(i, j) = New Expression(1.0)
                    End If
                Next
            Next
            Dim Term As New ExprMatrix(Identity)
            Dim k As Int64 = 1

            '        infinity
            '         --
            ' exp(A)= \   (1/k)!* A^k = I + A/1! + A^2/2! + A^3/3! + ...
            '         /
            '         --
            '        k=0
            '
            ' Just an approximation:
            Do
                rMtx += Term
                Term *= Me / New ExprMatrix(k)
                k += 1D
            Loop While k < 27 ' enough?
        Catch ex As Exception
            Throw
        End Try
        Return rMtx
    End Function
    Public ReadOnly Property getLastExpr() As Expression
        Get
            Return Item(Me.Rows - 1, Me.Columns(Me.Rows - 1) - 1)
        End Get
    End Property
    Public Function opReal() As ExprMatrix
        Dim eM As New ExprMatrix
        Try
            ReDim eM.vExpr(Rows - 1)
            For i As Int32 = 0 To Rows - 1
                ReDim Preserve eM.vExpr(i)(Columns(i) - 1)
                For j = 0 To Columns(i) - 1
                    eM.Item(i, j) = New Expression(0.0)
                    For Each t In Item(i, j).t
                        Dim t1 As New ExprTerm(t)
                        t1.Cf.pIm = New Rational(0.0)
                        eM.Item(i, j).t.Add(New ExprTerm(t1))
                    Next
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eM
    End Function
    Public Function opImag() As ExprMatrix
        Dim eM As New ExprMatrix
        Try
            ReDim eM.vExpr(Rows - 1)
            For i As Int32 = 0 To Rows - 1
                ReDim Preserve eM.vExpr(i)(Columns(i) - 1)
                For j = 0 To Columns(i) - 1
                    eM.Item(i, j) = New Expression(0.0)
                    For Each t In Item(i, j).t
                        Dim t1 As New ExprTerm(t)
                        t1.Cf.pRe = New Rational(0.0)
                        eM.Item(i, j).t.Add(New ExprTerm(t1))
                    Next
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eM
    End Function
    Public Function ConvertrigonometricToExp() As ExprMatrix
        Dim eM As New ExprMatrix
        Try
            ReDim eM.vExpr(Rows - 1)
            For i As Int32 = 0 To Rows - 1
                ReDim Preserve eM.vExpr(i)(Columns(i) - 1)
                For j = 0 To Columns(i) - 1
                    eM.Item(i, j) = Item(i, j).ConvertTrigToExp
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eM
    End Function
    Sub replaceConstants()
        'For i As Int32 = 0 To vExpr.Length - 1
        '    For j = 0 To vExpr(i).Length - 1
        '        vExpr(i)(j).replaceConstants()
        '    Next
        'Next
    End Sub
    Public Overrides Function ToString() As String
        Return ToString_eMtx(False)
    End Function
    Public Enum matrixOutputFormat
        semicolonAndPipe
        html
        mathml
    End Enum
    Public Function ToString_eMtx(Format As matrixOutputFormat,
                              Optional numDecimals As Int32 = 15,
                              Optional sImg As String = "",
                              Optional culture As Globalization.CultureInfo = Nothing) As String
        Dim sRet As String = ""
        Dim bMathML As Boolean = G10.mathml
        Dim mode As Int32 = 0
        Try
            If sImg <> "i" AndAlso sImg <> "j" Then sImg = "i"
            If culture Is Nothing Then culture = G10.CI
            Dim bHasRowsOrColumns As Boolean = False
            If Format <> matrixOutputFormat.semicolonAndPipe Then
                mode = IIf(bMathML, 1, 2)
            Else
                G10.mathml = False
            End If
            For i As Int32 = 0 To Me.vExpr.Length - 1
                If vExpr(i) IsNot Nothing Then
                    For j = 0 To Me.vExpr(i).Length - 1
                        If vExpr(i)(j) IsNot Nothing Then
                            Dim s1 As String = vExpr(i)(j).ToStringExpression(numDecimals, sImg, culture)
                            If mode = 1 Then
                                'Dim s As String = "<math xmlns=""http://www.w3.org/1998/Math/MathML"">"
                                'sRet += s + s1 + "</math>"
                                sRet += s1
                            Else
                                sRet += s1
                            End If
                        Else
                            If mode Then sRet += "&nbsp;"
                        End If
                        If j < vExpr(i).Length - 1 Then sRet += ";"
                        If j Then bHasRowsOrColumns = True
                    Next
                    If i Then bHasRowsOrColumns = True
                Else
                    If mode Then sRet += "&nbsp;"
                End If
                If i < vExpr.Length - 1 Then sRet += "|"
            Next
            If mode Then

                Dim s As String = "<table><tr><td>"
                s += sRet.Replace(";", "</td><td>").Replace("|", "</td></tr><tr><td>")
                s += "</td></tr></table>"
                sRet = s
            End If
        Catch ex As Exception
        Finally
            G10.mathml = bMathML
        End Try
        Return sRet
    End Function
End Class
