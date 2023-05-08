Imports System.Text
Imports System.Text.RegularExpressions

<Serializable()> _
Public Class ExprMatrix
    Dim expr(-1)() As Expression
    Public cfg As Config = Config.cfg
    Public Sub New(cfg As Config, ByVal nRows As Int64, ByVal nCols As Int64)
        AddRowCol(nRows - 1, nCols - 1)
        'Me.cfg =cfg 
    End Sub
    Public Sub New(ByVal db As Double)
        Add(0, 0, db)
    End Sub
    Public Sub New(ByVal cjo As Complex)
        Add(0, 0, cjo)
    End Sub
    Public Sub New(ByVal pA As Polynomial)
        Add(0, 0, pA)
        'cfg = pA.cfg
    End Sub
    Public Sub New(ByVal expr As Expression)
        Add(0, 0, expr)
        'cfg = expr.cfg
    End Sub
    Public Sub New(ByVal eM As ExprMatrix)
        Add(eM)
        'cfg = eM.cfg
    End Sub
    Public Sub New(ByVal mtx As Matrix)
        Add(mtx)
        'If mtx.getVars IsNot Nothing Then
        '   cfg = mtx.getVars.cfg
        'End If
    End Sub
    Public Sub New(ByVal m As Match)
        AddRowCol(0, 0)
        expr(0)(0) = New Expression(m)
    End Sub
    Public Sub setExprTo(cfg As Config, ByVal rows As Int64, ByVal cols As Int64)
        ReDim Preserve expr(rows - 1)
        For i As Int32 = 0 To rows - 1
            ReDim Preserve expr(i)(cols - 1)
        Next
    End Sub
    'Public Shared Function parsePoly(ByVal sVar As String) As ExprMatrix
    '    Return New ExprMatrix(Polynomial.GetPolynomial(sVar))
    'End Function
    Public Shared Function AddFnAndArg0(ByVal mFn As Match, _
                                             ByVal eA As ExprMatrix) As ExprMatrix
        Dim eC As New ExprMatrix(mFn)
        Try
            eC.expr(0)(0).getArgs = New Expression() {New Expression(eA.expr(0)(0))}
        Catch ex As Exception
            Throw ex
        End Try
        Return eC
    End Function

    Public Sub Add(ByVal row As Int64, _
                               ByVal col As Int64, _
                               ByVal db As Double)
        Try
            AddRowCol(row, col)
            expr(row)(col) = New Expression(db)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub Add(ByVal row As Int64, _
                               ByVal col As Int64, _
                               ByVal cjo As Complex)
        Try
            AddRowCol(row, col)
            expr(row)(col) = New Expression(cjo)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub Add(ByVal row As Int64, _
                               ByVal col As Int64, _
                               ByVal m As Match)
        Try
            AddRowCol(row, col)
            expr(row)(col) = New Expression(m)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub Add(ByVal row As Int64, _
                               ByVal col As Int64, _
                               ByVal pA As Polynomial)
        Try
            AddRowCol(row, col)
            expr(row)(col) = New Expression(pA)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub Add(ByVal row As Int64, _
                               ByVal col As Int64, _
                               ByVal expr As Expression)
        Try
            AddRowCol(row, col)
            Me.expr(row)(col) = New Expression(expr)
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub Add(ByVal eM As ExprMatrix)
        Try
            If cfg Is Nothing Then
                cfg = eM.cfg
            End If
            AddRowCol(eM.Rows - 1, eM.Cols - 1)
            For i As Int64 = 0 To expr.Length - 1
                For j As Int64 = 0 To expr(i).Length - 1
                    Me.expr(i)(j) = New Expression(eM.expr(i)(j))
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub Add(ByVal mtx As Matrix)
        Try
            AddRowCol(mtx.vVect.Length - 1, mtx.vVect(0).vPoly.Length - 1)
            For i As Int64 = 0 To expr.Length - 1
                For j As Int64 = 0 To expr(i).Length - 1
                    Me.expr(i)(j) = New Expression(mtx.vVect(i).vPoly(j))
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Private Sub AddRowCol(ByVal row As Int64, ByVal col As Int64)
        Try
            Dim i As Int64 = expr.Length
            If row >= i Then
                ReDim Preserve expr(row)
            End If
            For i = 0 To row
                If expr(i) Is Nothing OrElse _
                expr(i).Length <= col Then
                    ReDim Preserve expr(i)(col)
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public ReadOnly Property Rows() As Int64
        Get
            Return Me.expr.Length
        End Get
    End Property
    Public ReadOnly Property Cols() As Int64
        Get
            If Me.expr.Length Then
                Return Me.expr(0).Length
            End If
            Return -1
        End Get
    End Property
    Public ReadOnly Property ColsInRow(Row As Int64) As Int64
        Get
            If Me.expr.Length Then
                If Row < Rows AndAlso Me.expr(Row) IsNot Nothing Then
                    Return Me.expr(Row).Length
                End If
            End If
            Return -1
        End Get
    End Property
    Public ReadOnly Property IsSystemOfEquations() As Boolean
        Get
            Try
                For i As Int64 = 0 To expr.Length - 1
                    For j As Int64 = 0 To expr(i).Length - 1
                        If expr(i)(j) Is Nothing OrElse _
                        Not expr(i)(j).IsEquation Then
                            Return False
                        End If
                    Next
                Next
            Catch ex As Exception
                Throw ex
            End Try
            Return True
        End Get
    End Property
    Public ReadOnly Property bHasEqsAndExpr() As Boolean
        Get
            Dim bHasEqs As Boolean = False
            Dim bHasExpr As Boolean = False
            Try
                For i As Int64 = 0 To expr.Length - 1
                    For j As Int64 = 0 To expr(i).Length - 1
                        If expr(i)(j) IsNot Nothing Then
                            If expr(i)(j).IsEquation Then
                                bHasEqs = True
                            Else
                                bHasExpr = True
                            End If
                        End If
                    Next
                Next
            Catch ex As Exception
                Throw ex
            End Try
            Return (bHasEqs AndAlso bHasExpr)
        End Get
    End Property
    Public Property getExpr(ByVal row As Int64, ByVal col As Int64) As Expression
        Get
            Return expr(row)(col)
        End Get
        Set(ByVal value As Expression)
            If value IsNot Nothing Then
                expr(row)(col) = value
                Me.cfg = value.cfg
            Else
                expr(row)(col) = Nothing
            End If
        End Set
    End Property
    Public Property getExprAtRow(ByVal row As Int64) As Expression()
        Get
            If row < expr.Length Then
                Return expr(row)
            End If
            Return Nothing
        End Get
        Set(value As Expression())
            expr(row) = value
        End Set
    End Property
    Public ReadOnly Property getVector(ByVal row As Int64) As Vector
        Get
            Dim v As New Vector
            Try
                ReDim v.vPoly(Me.expr(row).Length - 1)
                For i As Int64 = 0 To v.vPoly.Length - 1
                    v.vPoly(i) = New Polynomial(Me.expr(row)(i).getPolynomial)
                Next
            Catch ex As Exception
                v = Nothing
            End Try
            Return v
        End Get
    End Property
    Public ReadOnly Property getExprMtxFromRow(ByVal row As Int64) As ExprMatrix
        Get
            Dim v As New ExprMatrix(Me.cfg, 1, Me.Cols)
            Try
                For i As Int64 = 0 To Me.Cols - 1
                    v.getExpr(0, i) = New Expression(Me.expr(row)(i))
                Next
            Catch ex As Exception
                v = Nothing
            End Try
            Return v
        End Get
    End Property
    Public Function tryGetMatrix(ByRef mtx As Matrix) As Boolean
        Try
            If mtx Is Nothing Then
                mtx = New Matrix
            End If
            ReDim mtx.vVect(Me.Rows - 1)
            For i As Int64 = 0 To Me.Rows - 1
                mtx.vVect(i) = New Vector
                ReDim mtx.vVect(i).vPoly(Me.Cols - 1)
                For j As Int64 = 0 To Me.Cols - 1
                    If Not Me.expr(i)(j).IsPolynomial Then
                        Return False
                    End If
                    mtx.vVect(i).vPoly(j) = _
                        New Polynomial( _
                        Me.expr(i)(j).getPolynomial)
                Next
            Next
        Catch ex As Exception
            Return False
        End Try
        Return True
    End Function
    Public ReadOnly Property getMatrix() As Matrix
        Get
            Dim mtx As New Matrix
            Try
                Dim nR As Int64 = 0
                Dim i, j As Int64
                For i = 0 To Me.Rows - 1
                    ReDim Preserve mtx.vVect(nR)
                    mtx.vVect(nR) = New Vector
                    ReDim Preserve mtx.vVect(nR).vPoly(Me.Cols - 1)
                    For j = 0 To Me.Cols - 1
                        If Me.expr(i) IsNot Nothing AndAlso _
                        j < Me.expr(i).Length AndAlso _
                        Me.expr(i)(j) IsNot Nothing Then
                            Dim pA As Polynomial = _
                            Me.expr(i)(j).getPolynomial
                            If pA IsNot Nothing Then
                                mtx.vVect(nR).vPoly(j) = _
                                    New Polynomial( _
                                    Me.expr(i)(j).getPolynomial)
                            Else
                                Return Nothing
                            End If
                        Else
                            ReDim Preserve mtx.vVect(nR).vPoly(j - 1)
                            Exit For
                        End If
                    Next
                    If mtx.vVect(nR).vPoly.Length Then 'j >= Me.Cols Then
                        nR += 1
                    End If
                Next
            Catch ex As Exception
                mtx = Nothing
            End Try
            Return mtx
        End Get
    End Property
    Public ReadOnly Property getCurExpr() As Expression
        Get
            Return expr(Me.Rows - 1)(Me.Cols - 1)
        End Get
    End Property
    Public ReadOnly Property getAllVars() As String()
        Get
            Dim vAllVars(-1) As String, iv As Int64
            For i As Int64 = 0 To expr.Length - 1
                For j As Int64 = 0 To expr(i).Length - 1
                    If expr(i)(j) IsNot Nothing Then
                        Dim vVarsArg() As String = Nothing
                        expr(i)(j).getAllVars(vVarsArg)
                        If vVarsArg.Length Then
                            ReDim Preserve vAllVars(iv + vVarsArg.Length - 1)
                            Array.Copy(vVarsArg, 0, vAllVars, iv, vVarsArg.Length)
                            iv = vAllVars.Length
                        End If
                    End If
                Next
            Next
            If vAllVars.Length Then
                If iv > 1 Then
                    Array.Sort(vAllVars)
                    Dim vV(0) As String
                    iv = 0
                    vV(0) = vAllVars(0)
                    For i = 1 To vAllVars.Length - 1
                        If vAllVars(i) <> vV(iv) Then
                            iv += 1
                            ReDim Preserve vV(iv)
                            vV(iv) = vAllVars(i)
                        End If
                    Next
                    vAllVars = vV
                End If
            End If
            Return vAllVars
        End Get
    End Property
    Public Function IsReal() As Boolean
        Try
            For i As Int64 = 0 To expr.Length - 1
                For j As Int64 = 0 To expr(i).Length - 1
                    If Not expr(i)(j).IsReal Then
                        Return False
                    End If
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function
    Public Function IsComplex() As Boolean
        Try
            For i As Int64 = 0 To expr.Length - 1
                For j As Int64 = 0 To expr(i).Length - 1
                    If Not expr(i)(j).IsComplex Then
                        Return False
                    End If
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function
    Public Function IsPolynomial() As Boolean
        Try
            For i As Int64 = 0 To expr.Length - 1
                For j As Int64 = 0 To expr(i).Length - 1
                    If expr(i)(j) Is Nothing OrElse _
                    Not expr(i)(j).IsPolynomial Then
                        Return False
                    End If
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return True
    End Function
    Public Overloads Shared Operator -(ByVal eMa As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(eMa)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.expr(i)(j) = -eMc.expr(i)(j)
                Next
            Next
            eMc.cfg = eMa.cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator -(ByVal eMa As ExprMatrix, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            Dim sDetail As String = ""
            If eMa.cfg.bDetail Then
                sDetail = MathGlobal8.getTable(eMa.ToStringExprMtx(eMa.cfg), "navy") + _
                        " - " + MathGlobal8.getTable(eMb.ToStringExprMtx(eMa.cfg), "navy")
            End If
            eMc = New ExprMatrix(eMa)
            Dim i, j As Int64
            For i = 0 To eMc.Rows - 1
                For j = 0 To eMc.Cols - 1
                    eMc.expr(i)(j) -= eMb.expr(i)(j)
                Next
            Next
            Dim sErr As String = String.Empty
            Dim oVars As VarsAndFns = Nothing
            'Dim mtx As Matrix = ReduceExprUsingPolynomials.ReduceUsingPolynomials(eMc, oVars)
            'Dim sMsg As String = String.Empty
            'eMc = ReduceExprUsingPolynomials.reducedPolynToExprMatrix(mtx, oVars)
            'For i = 0 To eMc.Rows - 1
            '    For j = 0 To eMc.Cols - 1
            '        If eMc.getExpr(i, j).IsComplex Then
            '        ElseIf eMc.getExpr(i, j).IsPolynomial Then
            '            eMc.getExpr(i, j).getPolynomial.tryReducePolyResto()
            '        Else

            '            Dim expr As Expression = _
            '                New Expression(eMc.getExpr(i, j))
            '            expr.reduce()
            '            eMc.getExpr(i, j) = rup.ReduceUsingPolynomials(expr)

            '        End If
            '    Next
            'Next

            If eMa.cfg.bDetail Then
                eMa.cfg.oDetail.Add(sDetail + " = " + _
                    MathGlobal8.getTable(eMc.ToStringExprMtx(eMa.cfg), "navy"))
            End If
            eMc.cfg = eMa.cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator -(ByVal expr As Expression, ByVal eMa As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(-eMa)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.getExpr(i, j) += expr
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator +(ByVal eMa As ExprMatrix, ByVal expr As Expression) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(eMa)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.getExpr(i, j) += expr
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator -(ByVal eMa As ExprMatrix, ByVal expr As Expression) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(eMa)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.getExpr(i, j) -= expr
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator +(ByVal expr As Expression, ByVal eMb As ExprMatrix) As ExprMatrix
        ' see http://en.wikipedia.org/wiki/Row_echelon_form
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(eMb)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.getExpr(i, j) += expr
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator +(ByVal eMa As ExprMatrix, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            Dim sDetail As String = ""
            If eMa.cfg.bDetail Then
                sDetail = MathGlobal8.getTable(eMa.ToStringExprMtx(eMa.cfg), "navy") + _
                        " + " + MathGlobal8.getTable(eMb.ToStringExprMtx(eMa.cfg), "navy")
            End If
            eMc = New ExprMatrix(eMa)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.expr(i)(j) += eMb.expr(i)(j)
                Next
            Next
            Dim sErr As String = String.Empty
            Dim oVars As VarsAndFns = Nothing
            'Dim mtx As Matrix = ReduceExprUsingPolynomials.ReduceUsingPolynomials(eMc, oVars)
            'eMc = New ExprMatrix(mtx)
            'eMc = ReduceExprUsingPolynomials.reducedPolynToExprMatrix(eMc, oVars)
            If eMa.cfg.bDetail Then
                eMa.cfg.oDetail.Add(sDetail + " = " + _
                    MathGlobal8.getTable(eMc.ToStringExprMtx(eMa.cfg), "navy"))
            End If
            eMc.cfg = eMa.cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator *(ByVal expr As Expression, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(eMb)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.getExpr(i, j) *= expr
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator *(ByVal eMa As ExprMatrix, ByVal expr As Expression) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(eMa)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.getExpr(i, j) *= expr
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator *(ByVal eMa As ExprMatrix, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            Dim sDetail As String = ""
            If eMa.cfg.bDetail Then
                sDetail = MathGlobal8.getTable("(" + eMa.ToStringExprMtx(eMa.cfg) + ")", "navy") +
                        " * " + MathGlobal8.getTable("(" + eMb.ToStringExprMtx(eMa.cfg) + ")", "navy")
            End If
            Dim bDoGcd As Boolean = Polynomial.bDoGCD
            Polynomial.bDoGCD = False
            eMc = New ExprMatrix(eMa.cfg, eMa.Rows, eMb.Cols)
            If eMa.Rows = 1 AndAlso eMa.Cols = 1 Then
                eMc = New ExprMatrix(eMb)
                For i As Int64 = 0 To eMb.Rows - 1
                    For j As Int64 = 0 To eMb.Cols - 1
                        eMc.expr(i)(j) *= eMa.expr(0)(0)
                    Next
                Next
            ElseIf eMb.Rows = 1 AndAlso eMb.Cols = 1 Then
                eMc = New ExprMatrix(eMa)
                For i As Int64 = 0 To eMa.Rows - 1
                    For j As Int64 = 0 To eMa.Cols - 1
                        eMc.expr(i)(j) *= eMb.expr(0)(0)
                    Next
                Next
            Else
                For i As Int64 = 0 To eMa.Rows - 1
                    For j As Int64 = 0 To eMb.Cols - 1
                        eMc.expr(i)(j) = New Expression(0.0)
                        For k As Int64 = 0 To eMb.Rows - 1
                            eMc.expr(i)(j) += (eMa.expr(i)(k) * eMb.expr(k)(j))
                        Next
                    Next
                Next
            End If
            Polynomial.bDoGCD = bDoGcd

            'Dim oVar As New VarsAndFns(eMa.cfg)
            'Dim mtx As Matrix = ReduceExprUsingPolynomials.ReduceUsingPolynomials(eMc, oVar)
            'Dim sMsg As String = String.Empty
            'eMc = New ExprMatrix(mtx)
            'eMc = ReduceExprUsingPolynomials.reducedPolynToExprMatrix(eMc, oVar)
            If eMa.cfg.bDetail Then
                eMa.cfg.oDetail.Add(sDetail + " = " + _
                    MathGlobal8.getTable(eMc.ToStringExprMtx(eMa.cfg), "navy"))
            End If
            eMc.cfg = eMa.cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator /(ByVal eMa As ExprMatrix, ByVal expr As Expression) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(eMa)
            For i As Int64 = 0 To eMc.Rows - 1
                For j As Int64 = 0 To eMc.Cols - 1
                    eMc.getExpr(i, j) /= expr
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator /(ByVal eMa As ExprMatrix, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            Dim sDetail As String = ""
            If eMa.cfg.bDetail Then
                sDetail = MathGlobal8.getTable("(" + eMa.ToStringExprMtx(eMa.cfg) + ")", "navy") + _
                        " / " + MathGlobal8.getTable("(" + eMb.ToStringExprMtx(eMa.cfg) + ")", "navy")
            End If
            If eMa.Rows * eMa.Cols + eMb.Rows * eMb.Cols = 2 Then
                eMc = New ExprMatrix(eMa.expr(0)(0) / eMb.expr(0)(0))
            Else
                eMc = eMa * (eMb ^ New ExprMatrix(-1))
            End If
            'Dim oVar As New VarsAndFns(eMa.cfg)
            'Dim mtx As Matrix = ReduceExprUsingPolynomials.ReduceUsingPolynomials(eMc, oVar)
            'Dim sMsg As String = String.Empty
            'eMc = New ExprMatrix(mtx)
            'eMc = ReduceExprUsingPolynomials.reducedPolynToExprMatrix(eMc, oVar)
            If eMa.cfg.bDetail Then
                eMa.cfg.oDetail.Add(sDetail + " = " + _
                    MathGlobal8.getTable(eMc.ToStringExprMtx(eMa.cfg), "navy"))
            End If
            eMc.cfg = eMa.cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Overloads Shared Operator ^(ByVal eMa As ExprMatrix, ByVal eMb As ExprMatrix) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            Dim sDetail As String = ""
            If eMa.cfg.bDetail Then
                sDetail = MathGlobal8.getTable("(" + eMa.ToStringExprMtx(eMa.cfg) + ")", "navy") + _
                        " ^ " + MathGlobal8.getTable("(" + eMb.ToStringExprMtx(eMa.cfg) + ")", "navy")
            End If
            If eMa.Rows * eMa.Cols + eMb.Rows * eMb.Cols = 2 Then
                eMc = New ExprMatrix(eMa.expr(0)(0) ^ eMb.expr(0)(0))
            ElseIf eMb.IsReal Then
                Dim db As Double = eMb.getVector(0).vPoly(0).cf(0).pRe.ToDouble
                Dim mtxA As Matrix = Nothing
                Dim oVar As New VarsAndFns(eMa.cfg)
                If False AndAlso eMa.tryGetMatrix(mtxA) Then
                    Dim mtxC As Matrix = mtxA ^ db
                    eMc = New ExprMatrix(mtxC)
                Else
                    Dim absDb As Double = Math.Abs(db)
                    If db < 0 Then
                        Dim InvM As ExprMatrix = Nothing
                        Dim det As Expression = _
                            eMa.opDeterminant(True, InvM)
                        eMc = InvM
                    ElseIf db = 0 Then
                        eMc = New ExprMatrix(eMa)
                        For i As Int64 = 0 To eMc.Rows - 1
                            For j As Int64 = 0 To eMc.Cols - 1
                                If i = j Then
                                    eMc.getExpr(i, j) = New Expression(0.0)
                                Else
                                    eMc.getExpr(i, j) = New Expression(1.0)
                                End If
                            Next
                        Next
                        Exit Try
                    Else
                        eMc = New ExprMatrix(eMa)
                    End If
                    'Exit Try

                    For i As Int64 = 2 To db
                        eMc *= eMa
                    Next
                    'Dim mtx As New Matrix(0.0) ' = ReduceExprUsingPolynomials.ReduceUsingPolynomials(eMc, oVar)
                    'Dim j As Int64
                    'Dim rup As New ReduceExprUsingPolynomials(oVar)
                    'ReDim mtx.vVect(eMa.Rows - 1)
                    'For i = 0 To eMa.Rows - 1
                    '    mtx.vVect(i) = New Vector(0.0)
                    '    ReDim Preserve mtx.vVect(i).vPoly(eMa.Cols - 1)
                    '    For j = 0 To eMa.Cols - 1
                    '        Dim expr As Expression = _
                    '            New Expression(eMa.getExpr(i, j))
                    '        'Dim generatedPoly As Expression = rup.convertToPolynomial(expr)
                    '        Dim aux As RUPaux = RUPaux.exprToRUPaux(expr, oVar)
                    '        'mtx.vVect(i).vPoly(j) = generatedPoly.getPolynomial
                    '        mtx.vVect(i).vPoly(j) = aux.getPolynomial
                    '    Next
                    'Next
                    'Dim sMsg As String = String.Empty
                    'Dim invMtx As Matrix = _
                    '    Functions.Inverse_Matrix_GaussPivotTotal_poly(mtx, sMsg)
                    'eMc = New ExprMatrix(mtx)

                    'For i = 0 To eMa.Rows - 1
                    '    For j = 0 To eMa.Cols - 1
                    '        eMc.getExpr(i, j) = invMtx.vVect(i).vPoly(j).evalMultiCjoToExpr(oVar)
                    '    Next
                    'Next
                End If
            Else
                Throw New Exception(msg8.num(13)) ' n/a
            End If
            If eMa.cfg.bDetail Then
                eMa.cfg.oDetail.Add(sDetail + " = " + _
                    MathGlobal8.getTable(eMc.ToStringExprMtx(eMa.cfg), "navy"))
            End If
            eMc.cfg = eMa.cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Operator
    Public Function opEchelonForm(Optional ByRef B As ExprMatrix = Nothing) As ExprMatrix
        Dim echelon As ExprMatrix = Nothing
        Try
            Dim oVar As VarsAndFns = Nothing
            'Dim eM As New ExprMatrix(Me)
            'SystemOfEquations.resolveDeterminateSysOfEqs(eM, oVar, B)
            Dim M As New Matrix
            Matrix.TryParseMatrix(Me, M)
            echelon = New ExprMatrix(Matrix.opEchelonForm(M))
        Catch ex As Exception
            Throw New Exception(msg8.num(13))
        End Try
        Return echelon
    End Function
    Public Function opJordanForm() As ExprMatrix
        Dim Jordan As ExprMatrix
        Try
            ' see http://www.math.ucla.edu/~jlindquist/115B/JCF.pdf
            ' also http://www.math.utah.edu/~yplee/teaching/2270f14/Jordan_Form.pdf
            ' (i) matrix A = me
            ' (ii) Compute pA(x) = det(x*I - A), finding the roots and multiplicities
            '   of each root.
            '  a) opEigenValues() returns all the roots of det(A-x*I) 
            '    (i.e., the same roots as per det(x*I-A)):
            Dim eigenValues As ExprMatrix = opEigenValues(False)
            '   Roots are sorted, which implies that
            '   equal roots are consecutive in opEigenValues()
            '  b) find the multiplicities:
            Dim vRoot(0) As Complex, vMultiplicity(0) As Int64
            Dim i As Int64, iR As Int64 = 0
            vRoot(0) = New Complex(eigenValues.getExpr(0, 0).toComplex)
            vMultiplicity(0) = 1
            For i = 1 To eigenValues.Rows - 1
                If (vRoot(iR) - eigenValues.getExpr(i, 0).toComplex).IsZero Then
                    ' root is repeated and so, increment its multiplicity:
                    vMultiplicity(iR) += 1
                Else
                    ' a different root --> add to vRoot()
                    iR += 1
                    ReDim Preserve vRoot(iR), vMultiplicity(iR)
                    vMultiplicity(iR) = 1
                    vRoot(iR) = New Complex(eigenValues.getExpr(i, 0).toComplex)
                End If
            Next
            ' (iii) Compute the dimensions of N((A-lambda_k*I)^r) for r=1,2,...
            '    until dim(N((A-lambda_k*I)^r)) = multiplicity(k)
            '    Recall dim(N(...)) = n - rank((A-lambda_k*I)^r) and
            '    A is a nxn matrix.
            Dim n As Int64 = Me.Rows
            Dim vN(iR)() As Int64
            ' Just to define the Jordan exprMatrix instance (diagonal entries will be
            ' overwritten):
            Jordan = Identity(cfg, n)
            Dim curEntry As Int64 = 0
            For k As Int64 = 0 To iR   ' for each lambda_k...
                ReDim Preserve vN(k)(0)
                ' compute vN(k)(0) = dim(N(A-lambda_k)) = n -rank(...
                Dim AminusLambda As New ExprMatrix( _
                        Me - New Expression(vRoot(k)) _
                        * Identity(cfg, n))
                vN(k)(0) = n - AminusLambda.opRank
                If vN(k)(0) = vMultiplicity(k) Then
                    ' One diagonal block of vMulitiplicity(k) size and
                    ' vRoot(k) at each diagonal entry:
                    For i = 0 To vMultiplicity(k) - 1
                        Jordan.getExpr(curEntry, curEntry) = New Expression(vRoot(k))
                        curEntry += 1
                        If curEntry >= Rows Then Exit Try
                    Next
                Else
                    Dim ivN As Int64 = 0
                    ' vN(k)(0) < vN(k)(1) < ... < vN(k)(Ln) = multiplicity(k)
                    ' Because the sequence increments strictly, the last
                    ' computation (vN(k)(Ln)) can be omitted:
                    Do While vN(k)(ivN) < vMultiplicity(k) - 1
                        ivN += 1
                        ReDim Preserve vN(k)(ivN)
                        ' increment AminusLambda power:
                        AminusLambda *= AminusLambda
                        vN(k)(ivN) = n - AminusLambda.opRank
                    Loop
                    If vN(k)(ivN) < vMultiplicity(k) Then
                        ' last omitted computation:
                        ivN += 1
                        ReDim Preserve vN(k)(ivN)
                        vN(k)(ivN) = vMultiplicity(k)
                    End If
                    ' (iv) The sequence vN(k)(1)-vN(k)(0) = minimum #of blocks of size 1
                    '      The sequence vN(k)(2)-vN(k)(1) = minimum #of blocks of size 2
                    '       "    "      vN(k)(i+1)-vN(k)(i) =  "     "    "     "  "  i+1
                    Dim d(ivN) As Int64
                    Dim remainigBlocks As Int64 = vMultiplicity(k)
                    For i = 1 To ivN
                        d(i) = vN(k)(i) - vN(k)(i - 1)
                    Next
                    Dim nBlocks(ivN), vBlockSize(ivN) As Int64
                    nBlocks(ivN) = d(ivN)
                    vBlockSize(ivN) = ivN + 1
                    remainigBlocks -= nBlocks(ivN) * vBlockSize(ivN)
                    For i = ivN - 1 To 1 Step -1
                        nBlocks(i) = d(i) - nBlocks(i + 1)
                        vBlockSize(i) = i + 1
                        remainigBlocks -= nBlocks(i) * vBlockSize(i)
                    Next
                    If remainigBlocks Then
                        ' Blocks of size = 1
                        ' OneSize diagonal entries
                        ' vRoot(k) at each diagonal entry:
                        For i = 0 To remainigBlocks - 1
                            Jordan.getExpr(curEntry, curEntry) = New Expression(vRoot(k))
                            curEntry += 1
                            If curEntry >= Rows Then GoTo sigI
                        Next
                    End If
                    For j As Int64 = 0 To ivN
                        For iBlock As Int64 = 1 To nBlocks(j) ' nBlocks(j) of size = vBlockSize(j)
                            For i = 0 To vBlockSize(j) - 1 ' a block of size = vBlockSize(j)
                                Jordan.getExpr(curEntry, curEntry) = New Expression(vRoot(k))
                                If curEntry + 1 >= Cols Then GoTo sigI
                                If i < vBlockSize(j) - 1 AndAlso _
                                curEntry + 1 < Cols Then
                                    ' if not the last row, set value '1' to
                                    ' the entry at the right of the diagonal (i.e.
                                    ' at the superdiagonal):
                                    Jordan.getExpr(curEntry, curEntry + 1) = _
                                        New Expression(1.0)
                                End If
                                curEntry += 1
                                If curEntry >= Rows Then Exit Try
                            Next
                        Next
                    Next
                End If
sigI:
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return Jordan
    End Function
    Public Function opRank() As Int64
        Dim r As Int64 = 0
        Try
            Dim Echelon As New ExprMatrix(Me)
            Echelon = opEchelonForm()
            Dim i, j As Int64
            For i = Echelon.Rows - 1 To 0 Step -1
                For j = 0 To Echelon.Cols - 1
                    If Not Echelon.getExpr(i, j).IsReal OrElse _
                    Echelon.getExpr(i, j).toDouble <> 0 Then
                        r = i + 1
                        Exit Try
                    End If
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return r
    End Function
    Public Function opTrace() As ExprMatrix
        ' see http://en.wikipedia.org/wiki/Trace_(linear_algebra)
        Dim r As ExprMatrix = Nothing
        Try
            Dim expr As New Expression(0.0)
            expr.cfg = Me.cfg
            For i = 0 To Rows - 1
                expr += getExpr(i, i)
            Next
            r = New ExprMatrix(expr)
        Catch ex As Exception
            Throw ex
        End Try
        Return r
    End Function
    Public Shared Function Identity(cfg As Config, rows As Int64, Optional cols As Int64 = 0) As ExprMatrix
        ' see http://en.wikipedia.org/wiki/Identity_matrix
        Dim emIdentity As ExprMatrix = Nothing
        Try
            If cols = 0 Then cols = rows
            emIdentity = New ExprMatrix(cfg, rows, cols)
            For j As Int64 = 0 To rows - 1
                For k As Int64 = 0 To cols - 1
                    If j = k Then
                        emIdentity.getExpr(j, k) = New Expression(1.0)
                    Else
                        emIdentity.getExpr(j, k) = New Expression(0.0)
                    End If
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return emIdentity
    End Function
    Public Shared Function Zero(cfg As Config, rows As Int64, Optional cols As Int64 = 0) As ExprMatrix
        Dim emIdentity As ExprMatrix = Nothing
        Try
            If cols = 0 Then cols = rows
            emIdentity = New ExprMatrix(cfg, rows, cols)
            For j As Int64 = 0 To rows - 1
                For k As Int64 = 0 To cols - 1
                    emIdentity.getExpr(j, k) = New Expression(0.0)
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return emIdentity
    End Function
    Public Function opEigenValues(bReturnAsDiagonalMatrix As Boolean) As ExprMatrix
        Dim eMtx As ExprMatrix = Nothing
        Try
            ' Finding the eigenvalues of matrix A, is
            ' equivalent to find the roots of det(A-lambda*I), where I
            ' is the identity matrix. (Do a search for eigenvalues).
            If Me.Rows <> Me.Cols Then
                Throw New Exception(msg8.msg(41, dspNumError:=False)) ' matrix is not squared
            End If
            Dim lamda As New Expression(Polynomial.GetPolynomial("_λ"))
            ' Presuming the determinant will be a polynomial:
            Dim eMtx1 As ExprMatrix = (Me - lamda * Identity(cfg, Me.Rows))
            Dim eDet As Expression = eMtx1.opDeterminant
            Dim Pa As Polynomial = eDet.getPolynomial
            Dim roots() As Complex = Polynomial.opRoots(Pa).cjo
            Dim rows As Int64 = roots.Length
            Dim cols As Int64 = rows
            If Not bReturnAsDiagonalMatrix Then
                cols = 1
                eMtx = New ExprMatrix(cfg, rows, cols)
                For i As Int64 = 0 To rows - 1
                    eMtx.getExpr(i, 0) = New Expression(roots(i))
                Next
            Else
                eMtx = New ExprMatrix(cfg, rows, cols)
                For i As Int64 = 0 To rows - 1
                    For j As Int64 = 0 To cols - 1
                        If i = j Then
                            eMtx.getExpr(i, j) = New Expression(roots(i))
                        Else
                            eMtx.getExpr(i, j) = New Expression(0.0)
                        End If
                    Next
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return eMtx
    End Function
    Public Function opEigenVectors(Optional bNormalize As Boolean = False) As ExprMatrix
        Dim eMtx As ExprMatrix = Nothing
        Try
            Dim vVars() As String = getAllVars
            eMtx = New ExprMatrix(cfg, Rows, Cols)
            ' 1) Obtain the eigenvalues
            Dim eigenValues As ExprMatrix = opEigenValues(False)
            '   Roots are sorted, which implies that
            '   equal roots are consecutive in opEigenValues()
            Dim vRoot(0) As Complex, vMultiplicity(0) As Int64
            Dim i, j As Int64, iR As Int64 = 0
            vRoot(0) = New Complex(eigenValues.getExpr(0, 0).toComplex)
            vMultiplicity(0) = 1
            For i = 1 To eigenValues.Rows - 1
                If (vRoot(iR) - eigenValues.getExpr(i, 0).toComplex).IsZero Then
                    ' root is repeated and so, increment its multiplicity:
                    vMultiplicity(iR) += 1
                Else
                    ' a different root --> add to vRoot()
                    iR += 1
                    ReDim Preserve vRoot(iR), vMultiplicity(iR)
                    vMultiplicity(iR) = 1
                    vRoot(iR) = New Complex(eigenValues.getExpr(i, 0).toComplex)
                End If
            Next
            ' 2) Solve A*v = lambda * v for each lambda in vRoot()
            Dim curVector As Int64 = 0
            For i = 0 To iR
                ' (A-lambda*I)*v = 0
                Dim lambda As New Expression(vRoot(i))
                Dim eqMtx As ExprMatrix = Me - lambda * Identity(Me.cfg, Rows)
                Dim oVar As New VarsAndFns(cfg)
                Dim rank As Int64 = eqMtx.opRank
                Dim nullDim As Int64 = Me.Rows - rank
                If nullDim = 0 Then nullDim = 1
                Dim b As ExprMatrix = ExprMatrix.Zero(cfg, Rows, 1)
                For row As Int64 = 0 To Rows - 1
                    b.getExpr(row, 0) = New Expression(0.0)
                Next
                Dim eqMtx1 As New ExprMatrix(eqMtx)
                SystemOfEquations.resolveDeterminateSysOfEqs(eqMtx1, oVar, b)
                Dim eigenVector As New ExprMatrix(cfg, Rows, 1)
                Dim iM As Int64 = 0
                Dim svVar() As String = Split(oVar.ToString, vbCrLf)
                For j = 0 To Cols - 1
                    Dim nom As String = "x" + j.ToString
                    Dim id As Int64 = oVar.getVarIDByName(nom)
                    Dim valEMtx As ExprMatrix = oVar.getValueByID(id)
                    If valEMtx Is Nothing Then
                        Dim j1 As Int64
                        For j1 = 0 To svVar.Length - 1
                            Dim e1() As String = Split(svVar(j1), "=")
                            If e1.Length > 1 AndAlso InStr(e1(1), nom) Then
                                Exit For
                            End If
                        Next
                        oVar.setValue(id, New ExprMatrix(1.0))
                    End If
                Next
                For j = 0 To Cols - 1
                    Try
                        oVar.setValue(j, New ExprMatrix(
                            oVar.getValueByID(j).getExpr(0, 0).evalExpression(Nothing, oVar)))
                    Catch ex As Exception

                    End Try
                Next
                For j = 0 To Cols - 1
                    Dim nom As String = "x" + j.ToString
                    Dim id As Int64 = oVar.getVarIDByName(nom)
                    Dim valEMtx As ExprMatrix = oVar.getValueByID(id)
                    If Not valEMtx.IsComplex Then
                        oVar.setValue(id, New ExprMatrix(1.0))
                        Exit For
                    End If
                Next
                For j = 0 To Cols - 1
                    oVar.setValue(j, New ExprMatrix(
                        oVar.getValueByID(j).getExpr(0, 0).evalExpression(Nothing, oVar)))
                Next
                For iNull As Int64 = 1 To nullDim
                    For j = 0 To Cols - 1
                        Dim nom As String = "x" + j.ToString
                        Dim id As Int64 = oVar.getVarIDByName(nom)
                        Dim cExpr As Expression = oVar.getValueByID(id).getExpr(0, 0)
                        eMtx.getExpr(j, curVector) = New Expression(cExpr)
                        eigenVector.getExpr(j, 0) = eMtx.getExpr(j, curVector)
                    Next
                    curVector += 1
                    iM += 1
                Next

                ' generalized eigenvectors, if any:
                For iM2 As Int64 = iM To vMultiplicity(i) - 1
                    'Matrix.TryParseMatrix(echelon, b1)
                    'echelon = eqMtx.opEchelonForm(b1)
                    'Dim rank1 As Int64 = echelon.opRank
                    oVar = Nothing
                    SystemOfEquations.resolveDeterminateSysOfEqs(eqMtx, oVar, eigenVector)
                    svVar = Split(oVar.ToString, vbCrLf)
                    For j = 0 To Cols - 1
                        Dim nom As String = "x" + j.ToString
                        Dim id As Int64 = oVar.getVarIDByName(nom)
                        Dim valEMtx As ExprMatrix = oVar.getValueByID(id)
                        If valEMtx Is Nothing Then
                            Dim j1 As Int64
                            For j1 = 0 To svVar.Length - 1
                                Dim e1() As String = Split(svVar(j1), "=")
                                If e1.Length > 1 AndAlso InStr(e1(1), nom) Then
                                    Exit For
                                End If
                            Next
                            oVar.setValue(id, New ExprMatrix(1.0))
                        End If
                    Next
                    For j = 0 To Cols - 1
                        oVar.setValue(j, New ExprMatrix(
                            oVar.getValueByID(j).getExpr(0, 0).evalExpression(Nothing, oVar)))
                    Next
                    For j = 0 To Cols - 1
                        Dim cExpr As Expression = oVar.getValueByID(j).getExpr(0, 0)
                        eMtx.getExpr(j, curVector) = New Expression(cExpr)
                        eigenVector.getExpr(j, 0) = eMtx.getExpr(j, curVector)
                    Next
                    'rank += rank1
                Next
            Next
            For j = 0 To eMtx.Cols - 1
                If eMtx.getExpr(0, j) Is Nothing Then
                    eMtx.setExprTo(cfg, eMtx.Rows, j)
                    Exit For
                End If
            Next

            ' normalize
            If bNormalize Then
                For j = 0 To eMtx.Cols - 1
                    Dim norm As Double = 0.0
                    For i = 0 To eMtx.Rows - 1
                        norm += eMtx.getExpr(i, j).toDouble ^ 2
                    Next
                    norm = norm ^ 0.5
                    Dim normExpr As New Expression(norm)
                    For i = 0 To eMtx.Rows - 1
                        eMtx.getExpr(i, j) /= normExpr
                    Next
                Next
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return eMtx
    End Function

    Public Function opEigenVectorsBAD() As ExprMatrix
        Dim eMtx As ExprMatrix = Nothing
        Try
            Dim vVars() As String = getAllVars
            eMtx = New ExprMatrix(cfg, Rows, Cols)
            ' 1) Obtain the eigenvalues
            Dim eigenValues As ExprMatrix = opEigenValues(False)
            '   Roots are sorted, which implies that
            '   equal roots are consecutive in opEigenValues()
            Dim vRoot(0) As Complex, vMultiplicity(0) As Int64
            Dim i, j As Int64, iR As Int64 = 0
            vRoot(0) = New Complex(eigenValues.getExpr(0, 0).toComplex)
            vMultiplicity(0) = 1
            For i = 1 To eigenValues.Rows - 1
                If (vRoot(iR) - eigenValues.getExpr(i, 0).toComplex).IsZero Then
                    ' root is repeated and so, increment its multiplicity:
                    vMultiplicity(iR) += 1
                Else
                    ' a different root --> add to vRoot()
                    iR += 1
                    ReDim Preserve vRoot(iR), vMultiplicity(iR)
                    vMultiplicity(iR) = 1
                    vRoot(iR) = New Complex(eigenValues.getExpr(i, 0).toComplex)
                End If
            Next
            ' 2) Solve A*v = lambda * v for each lambda in vRoot()
            Dim curVector As Int64 = 0
            For i = 0 To iR
                ' (A-lambda*I)*v = 0
                Dim lambda As New Expression(vRoot(i))
                Dim eqMtx As ExprMatrix = Me - lambda * Identity(Me.cfg, Rows)
                Dim oVar As New VarsAndFns(cfg)
                Dim rank As Int64 = eqMtx.opRank
                Dim nullDim As Int64 = Me.Rows - rank
                If nullDim = 0 Then nullDim = 1
                Dim b As ExprMatrix = ExprMatrix.Zero(cfg, Rows, 1)
                For row As Int64 = 0 To Rows - 1
                    b.getExpr(row, 0) = New Expression(0.0)
                Next
                'Dim eqMtx1 As New ExprMatrix(eqMtx)
                'SystemOfEquations.resolveDeterminateSysOfEqs(eqMtx1, oVar, b)
                Dim mtx As Matrix = Nothing
                Matrix.TryParseMatrix(Me, mtx)
                For k1 As Int32 = 0 To mtx.vVect.Length - 1
                    mtx.vVect(k1).vPoly(k1) -= New Polynomial(vRoot(i))
                Next
                Dim ech As Matrix = Matrix.opEchelonForm(mtx)
                Dim iFin As Int32 = Math.Min(ech.vVect.Length - 1, ech.vVect(0).vPoly.Length - 1)
                Dim expr As Expression = Nothing
                For j = iFin To 0 Step -1
                    expr = New Expression(0)
                    For k = iFin To j Step -1
                        expr += New Expression(ech.vVect(j).vPoly(j) * Polynomial.GetPolynomial("x" + k.ToString))
                        expr = expr.evalExprToExpr(oVar)
                    Next
                    Dim vR(1) As Expression
                    If expr.tryToIsolateToExpression("x" + j.ToString, vR) Then
                        oVar.AddVar("x" + j.ToString, vR(0))
                        eMtx.getExpr(i, j) = vR(0)
                    Else
                        oVar.AddVar("x" + j.ToString, New Expression(1.0))
                        eMtx.getExpr(i, j) = New Expression(1.0)
                    End If
                Next


                'Dim eigenVector As New ExprMatrix(cfg, Rows, 1)
                'Dim iM As Int64 = 0
                'Dim svVar() As String = Split(oVar.ToString, vbCrLf)
                'For j = 0 To Cols - 1
                '    Dim nom As String = "x" + j.ToString
                '    Dim id As Int64 = oVar.getVarIDByName(nom)
                '    Dim valEMtx As ExprMatrix = oVar.getValueByID(id)
                '    If valEMtx Is Nothing Then
                '        Dim j1 As Int64
                '        For j1 = 0 To svVar.Length - 1
                '            Dim e1() As String = Split(svVar(j1), "=")
                '            If e1.Length > 1 AndAlso InStr(e1(1), nom) Then
                '                Exit For
                '            End If
                '        Next
                '        oVar.setValue(id, New ExprMatrix(1.0))
                '    End If
                'Next
                'For j = 0 To Cols - 1
                '    Try
                '        oVar.setValue(j, New ExprMatrix(
                '            oVar.getValueByID(j).getExpr(0, 0).evalExpression(Nothing, oVar)))
                '    Catch ex As Exception

                '    End Try
                'Next
                'For j = 0 To Cols - 1
                '    Dim nom As String = "x" + j.ToString
                '    Dim id As Int64 = oVar.getVarIDByName(nom)
                '    Dim valEMtx As ExprMatrix = oVar.getValueByID(id)
                '    If Not valEMtx.IsComplex Then
                '        oVar.setValue(id, New ExprMatrix(1.0))
                '        Exit For
                '    End If
                'Next
                'For j = 0 To Cols - 1
                '    oVar.setValue(j, New ExprMatrix(
                '        oVar.getValueByID(j).getExpr(0, 0).evalExpression(Nothing, oVar)))
                'Next
                'For iNull As Int64 = 1 To nullDim
                '    For j = 0 To Cols - 1
                '        Dim nom As String = "x" + j.ToString
                '        Dim id As Int64 = oVar.getVarIDByName(nom)
                '        Dim cExpr As Expression = oVar.getValueByID(id).getExpr(0, 0)
                '        eMtx.getExpr(j, curVector) = New Expression(cExpr)
                '        eigenVector.getExpr(j, 0) = eMtx.getExpr(j, curVector)
                '    Next
                '    curVector += 1
                '    iM += 1
                'Next

                ' generalized eigenvectors, if any:
                'For iM2 As Int64 = iM To vMultiplicity(i) - 1
                '    oVar = Nothing
                '    SystemOfEquations.resolveDeterminateSysOfEqs(eqMtx, oVar, eigenVector)
                '    svVar = Split(oVar.ToString, vbCrLf)
                '    For j = 0 To Cols - 1
                '        Dim nom As String = "x" + j.ToString
                '        Dim id As Int64 = oVar.getVarIDByName(nom)
                '        Dim valEMtx As ExprMatrix = oVar.getValueByID(id)
                '        If valEMtx Is Nothing Then
                '            Dim j1 As Int64
                '            For j1 = 0 To svVar.Length - 1
                '                Dim e1() As String = Split(svVar(j1), "=")
                '                If e1.Length > 1 AndAlso InStr(e1(1), nom) Then
                '                    Exit For
                '                End If
                '            Next
                '            oVar.setValue(id, New ExprMatrix(1.0))
                '        End If
                '    Next
                '    For j = 0 To Cols - 1
                '        oVar.setValue(j, New ExprMatrix(
                '            oVar.getValueByID(j).getExpr(0, 0).evalExpression(Nothing, oVar)))
                '    Next
                '    For j = 0 To Cols - 1
                '        Dim cExpr As Expression = oVar.getValueByID(j).getExpr(0, 0)
                '        eMtx.getExpr(j, curVector) = New Expression(cExpr)
                '        eigenVector.getExpr(j, 0) = eMtx.getExpr(j, curVector)
                '    Next
                '    curVector += 1
                'Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return eMtx
    End Function

    Public Function opMenor(iRow As Int64, iCol As Int64) As ExprMatrix
        Dim i, j As Int64
        Dim i1 As Int64 = 0
        Dim Adj As New ExprMatrix(cfg, Rows - 1, Cols - 1)
        For i = 0 To Rows - 1
            If i <> iRow Then ' exclude row = iRow
                Dim j1 As Int64 = 0
                For j = 0 To Cols - 1
                    If j <> iCol Then ' exclude col = iCol
                        Adj.getExpr(i1, j1) = New Expression(Me.getExpr(i, j))
                        j1 += 1
                    End If
                Next
                i1 += 1
            End If
        Next
        Return Adj
    End Function
    Public Sub opInterChangeRowOrCol(interchageI As Int64, withJ As Int64, bAreRows As Boolean)
        Dim i, j As Int64
        If bAreRows Then
            For j = 0 To Cols - 1
                Dim aux As Expression = getExpr(interchageI, j)
                getExpr(interchageI, j) = getExpr(withJ, j)
                getExpr(withJ, j) = aux
            Next
        Else
            For i = 0 To Rows - 1
                Dim aux As Expression = getExpr(i, interchageI)
                getExpr(i, interchageI) = getExpr(i, withJ)
                getExpr(i, withJ) = aux
            Next
        End If
    End Sub
    Public Function opDeterminant(
                Optional bGetInverseOrCofactorMtx As Boolean = False,
                Optional ByRef InvMtx As ExprMatrix = Nothing,
                Optional ByRef cofactorMtx As ExprMatrix = Nothing,
                Optional ByRef adjoint As ExprMatrix = Nothing) As Expression
        Dim retDetermExpr As Expression = Nothing
        Dim i, j As Int64
        Dim N As Int64 = Me.Rows
        Dim fact As Int64 = N
        Dim permI(N - 1), cInv(N - 1) As Int64
        Dim rup As New ReduceExprUsingPolynomials
        Dim cpyeMtx As New ExprMatrix(Me)
        Try

            ' see Leibniz formula at:
            ' http://en.wikipedia.org/wiki/Determinant

            Dim oVar As New VarsAndFns(Me.cfg)
            If Me.Rows > 1 Then
                Dim sgn As Int64 = 1
                Dim maxZeros As Int64 = 0
                Dim indexZeros As Int64 = 0
                For j = 0 To Cols - 1
                    Dim currZeros As Int64 = 0
                    For i = 0 To Rows - 1
                        Dim bZero As Boolean = True
                        If getExpr(i, j).IsPolynomial Then
                            Dim pa As Polynomial = getExpr(i, j).getPolynomial
                            If Not pa.isReal OrElse pa.ToDouble <> 0 Then
                                bZero = False
                            End If
                        Else
                            bZero = False
                        End If
                        If bZero Then
                            currZeros += 1
                        End If
                    Next
                    If currZeros > maxZeros Then
                        indexZeros = j
                        maxZeros = currZeros
                    End If
                Next
                If maxZeros Then
                    If indexZeros Then
                        opInterChangeRowOrCol(0, indexZeros, False)
                        sgn = -sgn
                    End If
                    For i = 0 To Rows - 1
                        Dim bNonZero As Boolean = False
                        If getExpr(i, 0).IsPolynomial Then
                            Dim pa As Polynomial = getExpr(i, 0).getPolynomial
                            If Not pa.isReal OrElse pa.ToDouble <> 0 Then
                                bNonZero = True
                            End If
                        Else
                            bNonZero = True
                        End If
                        If Not bNonZero Then
                            For j = i + 1 To Rows - 1
                                bNonZero = False
                                If getExpr(j, 0).IsPolynomial Then
                                    Dim pa As Polynomial = getExpr(j, 0).getPolynomial
                                    If Not pa.isReal OrElse pa.ToDouble <> 0 Then
                                        bNonZero = True
                                    End If
                                Else
                                    bNonZero = True
                                End If
                                If bNonZero Then
                                    opInterChangeRowOrCol(i, j, True)
                                    sgn = -sgn
                                    Exit For
                                End If
                            Next
                        End If
                    Next
                End If
                retDetermExpr = New Expression(getExpr(0, 0))
                Do
                    Dim bContinue As Boolean = False
                    For i = Rows - 1 To 0 Step -1
                        Dim bNonZero As Boolean = False
                        If getExpr(i, 0).IsPolynomial Then
                            Dim pa As Polynomial = getExpr(i, 0).getPolynomial
                            If Not pa.isReal OrElse pa.ToDouble <> 0 Then
                                bNonZero = True
                            End If
                        Else
                            bNonZero = True
                        End If
                        If bNonZero AndAlso i Then
                            Dim v As New ExprMatrix(getExprMtxFromRow(i - 1))
                            Dim v2 As New ExprMatrix(getExprMtxFromRow(i))
                            Dim ai As Expression = getExpr(i, 0)
                            Dim k As Int32 = i - 1
                            Dim aimn1 As Expression = getExpr(k, 0)
                            Do While k > 0 AndAlso aimn1.IsReal AndAlso aimn1.toDouble = 0.0
                                k -= 1
                                aimn1 = getExpr(k, 0)
                            Loop
                            If Not (aimn1.IsReal AndAlso aimn1.toDouble = 0.0) Then
                                If ai.IsPolynomial AndAlso aimn1.IsPolynomial Then
                                    Dim gcd As Polynomial = Polynomial.opGcd(ai.getPolynomial, aimn1.getPolynomial)
                                    If gcd IsNot Nothing Then
                                        ai /= New Expression(gcd)
                                        aimn1 /= New Expression(gcd)
                                    End If
                                End If
                                retDetermExpr /= aimn1
                                v *= ai
                                v2 *= aimn1
                                v2 -= v
                                For j = 0 To Cols - 1
                                    getExpr(i, j) = rup.ReduceUsingPolynomials(v2.getExpr(0, j))
                                Next
                                bContinue = True
                            End If
                        End If
                    Next
                    If Not bContinue Then
                        Exit Do
                    End If
                Loop
                Dim menor As ExprMatrix = opMenor(0, 0)
                retDetermExpr *= menor.opDeterminant
                If sgn = -1 Then
                    retDetermExpr = -retDetermExpr
                End If
                'Return retDetermExpr
            Else
                ' Get all permutations fact = N!
                For i = N - 1 To 2 Step -1
                    fact *= i
                Next
                ' Get first permutation in permI:
                For i = 0 To N - 1
                    permI(i) = i
                Next

                retDetermExpr = New Expression(0.0)
                If True Then

                    ' Get a copy out from the 1st permutation:
                    Dim permBis(permI.Length - 1) As Int64
                    Array.Copy(permI, permBis, permI.Length)

                    For a As Int64 = 1 To fact
                        Dim sgn As Int64 = 1
                        Dim curr As New Expression(1.0)
                        For i = 0 To permI.Length - 1
                            For j = 0 To i - 1
                                If permI(j) > permI(i) Then sgn *= -1
                            Next
                            curr *= getExpr(i, permI(i))
                        Next
                        If sgn = 1 Then
                            retDetermExpr += curr
                        Else
                            retDetermExpr -= curr
                        End If

                        ' Get next permutation in permI:
                        Dim sb2 As New List(Of Int64)
                        sb2.AddRange(permBis)
                        Dim div As Int64 = fact
                        For b As Int64 = N To 1 Step -1
                            div /= b
                            Dim index As Int64 = (a \ div) Mod b
                            permI(N - b) = sb2.Item(index)
                            sb2.RemoveAt(index)
                        Next
                    Next
                End If
            End If


        Catch ex As Exception
            Throw ex
        End Try
        Try
            If bGetInverseOrCofactorMtx Then
                Dim Adj(N - 1, N - 1) As ExprMatrix
                If N = 1 Then
                    Adj(0, 0) = New ExprMatrix(Me.getExpr(0, 0))
                Else
                    For iRow As Int64 = 0 To N - 1
                        For iCol As Int64 = 0 To N - 1
                            ' Get exprMatrix Adj(,) out from
                            ' 'me' where row 'iRow' and column 'iCol'
                            ' are being excluded:
                            Dim i1 As Int64 = 0
                            For i = 0 To N - 1
                                If i <> iRow Then ' exclude row = iRow
                                    Dim j1 As Int64 = 0
                                    For j = 0 To N - 1
                                        If j <> iCol Then ' exclude col = iCol
                                            If Adj(iRow, iCol) Is Nothing Then
                                                Adj(iRow, iCol) = New ExprMatrix( _
                                                    cfg, N - 1, N - 1)
                                            End If
                                            If (i + j) Mod 2 Then
                                                Adj(iRow, iCol).getExpr(i1, j1) =
                                                    New Expression(-cpyemtx.getExpr(i, j))
                                            Else
                                                Adj(iRow, iCol).getExpr(i1, j1) =
                                                    New Expression(cpyeMtx.getExpr(i, j))
                                            End If
                                            j1 += 1
                                        End If
                                    Next
                                    i1 += 1
                                End If
                            Next
                        Next
                    Next
                End If
                ' Obtain all the determinants of Adj(,)
                ' and assing to InvMtx:
                InvMtx = New ExprMatrix(cfg, N, N)
                For i = 0 To N - 1
                    For j = 0 To N - 1
                        InvMtx.getExpr(i, j) = Adj(i, j).opDeterminant
                    Next
                Next
                cofactorMtx = InvMtx
                adjoint = ExprMatrix.opTranspose(InvMtx)
                ' Finally, transpose InvMtx and divide each
                ' entry by the determinant (retExpr):
                InvMtx = ExprMatrix.opTranspose(InvMtx, retDetermExpr)
            End If

        Catch ex As Exception
            Throw ex
        End Try

        Return retDetermExpr
    End Function
    Public Shared Function opTranspose(ByVal eMa As ExprMatrix, _
               Optional divideBy As Expression = Nothing) As ExprMatrix
        Dim eMc As New ExprMatrix(eMa.cfg, eMa.Cols, eMa.Rows)
        Try
            Dim i As Int64
            For iCol As Int64 = 0 To eMa.Cols - 1
                For i = 0 To eMa.Rows - 1
                    If divideBy Is Nothing Then
                        eMc.getExpr(iCol, i) = _
                            New Expression(eMa.getExpr(i, iCol))
                    Else
                        eMc.getExpr(iCol, i) = _
                            eMa.getExpr(i, iCol) / divideBy
                    End If
                Next
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return eMc
    End Function

    Public Function opJacobian(ByVal vars As VarsAndFns) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(Me.cfg, Me.Rows, Me.Cols)
            Dim sVar(-1) As String
            Dim col As Int64
            sVar = Me.getAllVars
            If vars.getNamesList.Length Then
                sVar = vars.getNamesList
            End If
            For row As Int64 = 0 To Me.Rows - 1
                For col = 0 To Me.Cols - 1
                    Dim expr As Expression = Me.getExpr(row, col)
                    If expr IsNot Nothing Then
                        eMc.AddRowCol(row, col)
                        eMc.getExpr(row, col) = expr.opDeriv(sVar(col))
                    End If
                Next
            Next
            eMc.cfg = cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Function
    Public Shared Function opCrossProduct(ByVal eMa As ExprMatrix,
               Optional divideBy As Expression = Nothing) As ExprMatrix
        Dim eMc As New ExprMatrix(eMa.cfg, 1, 3) ' 1 row, 3 columns
        Try
            ' | i   j   k  |
            '
            ' |a00 a01 a02 |
            ' |a10 a11 a12 |
            Dim a00 As New Expression(eMa.getExpr(0, 0))
            Dim a01 As New Expression(eMa.getExpr(0, 1))
            Dim a02 As New Expression(eMa.getExpr(0, 2))
            Dim a10 As New Expression(eMa.getExpr(1, 0))
            Dim a11 As New Expression(eMa.getExpr(1, 1))
            Dim a12 As New Expression(eMa.getExpr(1, 2))
            eMc.getExpr(0, 0) = a01 * a12 - a02 * a11
            eMc.getExpr(0, 1) = -(a00 * a12 - a02 * a10)
            eMc.getExpr(0, 2) = a00 * a11 - a01 * a10
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return eMc
    End Function
    Public Shared Function opDotProduct(ByVal eMa As ExprMatrix,
               Optional divideBy As Expression = Nothing) As ExprMatrix
        Dim eMc As New ExprMatrix(eMa.cfg, 1, 1) ' 1 row, 1 column
        Try
            'Dim i As Int64
            '
            ' |a00 a01 a02 |
            ' |a10 a11 a12 |
            Dim a00 As New Expression(eMa.getExpr(0, 0))
            Dim a01 As New Expression(eMa.getExpr(0, 1))
            Dim a02 As New Expression(eMa.getExpr(0, 2))
            Dim a10 As New Expression(eMa.getExpr(1, 0))
            Dim a11 As New Expression(eMa.getExpr(1, 1))
            Dim a12 As New Expression(eMa.getExpr(1, 2))
            eMc.getExpr(0, 0) = a00 * a10 + a01 * a11 + a02 * a12
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return eMc
    End Function
    Public Shared Function eval(ByVal eMa As ExprMatrix,
            Optional ByVal oVars As VarsAndFns = Nothing) As ExprMatrix
        Dim eMc As New ExprMatrix(eMa.cfg, eMa.Rows, eMa.Cols)
        Try
            Dim i As Int64
            For iCol As Int64 = 0 To eMa.Cols - 1
                For i = 0 To eMa.Rows - 1
                    eMc.getExpr(iCol, i) =
                        eMa.getExpr(i, iCol).evalExprToExpr(oVars)
                Next
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return eMc
    End Function
    Public Shared Function reduceUsingPolynomials(ByVal eMa As ExprMatrix) As ExprMatrix
        Dim eMc As New ExprMatrix(eMa.cfg, eMa.Rows, eMa.Cols)
        Try
            Dim rup As New ReduceExprUsingPolynomials
            Dim i As Int64
            For iCol As Int64 = 0 To eMa.Cols - 1
                For i = 0 To eMa.Rows - 1
                    eMc.getExpr(iCol, i) = _
                       rup.ReduceUsingPolynomials(eMa.getExpr(i, iCol))
                Next
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return eMc
    End Function

    Public Function opReal() As ExprMatrix
        ' Output: the same expression matrix, but
        ' only with the real values.
        Dim eMc As ExprMatrix = Nothing
        Dim rup As New ReduceExprUsingPolynomials
        Try
            eMc = New ExprMatrix(Me.cfg, 0, 0)
            Dim col As Int64
            For row As Int64 = 0 To Me.Rows - 1
                For col = 0 To Me.expr(row).Length - 1
                    Dim expr As Expression = Me.getExpr(row, col)
                    If expr IsNot Nothing Then
                        eMc.AddRowCol(row, col)
                        eMc.getExpr(row, col) = New Expression(0.0)
                        Dim bIsMn() As Boolean = Nothing
                        Dim vSum() As Expression = expr.exprToSummands(bIsMn)
                        Dim i As Int64
                        For i = 0 To vSum.Length - 1
                            vSum(i) = rup.ReduceUsingPolynomials(vSum(i))
                        Next
                        expr = Expression.summandsToExpr(vSum, bIsMn)
                        vSum = expr.exprToSummands(bIsMn)
                        For i = 0 To vSum.Length - 1
                            Dim bIsDiv(-1) As Boolean
                            Dim vFact() As Expression = vSum(i).exprToFactors(bIsDiv)
                            Dim j As Int64
                            Dim bIsReal As Boolean = True
                            For j = 0 To vFact.Length - 1
                                If vFact(j).IsComplex AndAlso
                                ((vFact(j).toComplex + Complex.i).IsZero OrElse
                                (vFact(j).toComplex - Complex.i).IsZero) Then
                                    bIsReal = False
                                    Exit For
                                ElseIf vFact(j).IsPolynomial Then
                                    Dim vPa() As Polynomial = vFact(j).getPolynomial.splitIntoTerms
                                    Dim Pa As Polynomial = vFact(j).getPolynomial
                                    Dim imag As New Polynomial(0.0)
                                    For k As Int64 = 0 To vPa.Length - 1
                                        For ic As Int64 = 0 To vPa(k).cf.Length - 1
                                            vPa(k).cf(ic).pIm = New Rational(0.0)
                                        Next
                                        imag += vPa(k)
                                    Next

                                    vFact(j) = New Expression(imag)
                                    If Pa.PolyResto IsNot Nothing Then
                                        vPa = Pa.PolyResto.splitIntoTerms
                                        Dim imagNum As New Polynomial(0.0)
                                        For k As Int64 = 0 To vPa.Length - 1
                                            For ic As Int64 = 0 To vPa(k).cf.Length - 1
                                                vPa(k).cf(ic).pim = New Rational(0.0)
                                            Next
                                            imagNum += vPa(k)
                                        Next
                                        vFact(j) += New Expression(imagNum / Pa.PolyDivisor)
                                    End If
                                End If
                            Next
                            If Not bIsReal Then
                            Else
                                vSum(i) = Expression.factorsToExpr(vFact, bIsDiv)
                                vSum(i) = rup.ReduceUsingPolynomials(vSum(i))
                                eMc.getExpr(row, col) += vSum(i)
                            End If
                        Next
                    End If
                Next
            Next
            eMc.cfg = cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Function
    Public Function opImag() As ExprMatrix
        ' Output: the same expression matrix, but
        ' only with the imaginary values.
        Dim eMc As ExprMatrix = Nothing
        Dim rup As New ReduceExprUsingPolynomials
        Try
            eMc = New ExprMatrix(Me.cfg, 0, 0)
            Dim col As Int64
            For row As Int64 = 0 To Me.Rows - 1
                For col = 0 To Me.expr(row).Length - 1
                    Dim expr As Expression = Me.getExpr(row, col)
                    If expr IsNot Nothing Then
                        eMc.AddRowCol(row, col)
                        eMc.getExpr(row, col) = New Expression(0.0)
                        Dim bIsMn() As Boolean = Nothing
                        Dim vSum() As Expression = expr.exprToSummands(bIsMn)
                        Dim i As Int64
                        For i = 0 To vSum.Length - 1
                            vSum(i) = rup.ReduceUsingPolynomials(vSum(i))
                        Next
                        expr = Expression.summandsToExpr(vSum, bIsMn)
                        vSum = expr.exprToSummands(bIsMn)
                        For i = 0 To vSum.Length - 1
                            Dim bIsDiv(-1) As Boolean
                            Dim vFact() As Expression = vSum(i).exprToFactors(bIsDiv)
                            Dim j As Int64
                            Dim bIsImag As Boolean = False
                            For j = 0 To vFact.Length - 1
                                If vFact(j).IsComplex AndAlso
                                ((vFact(j).toComplex + Complex.i).IsZero OrElse
                                (vFact(j).toComplex - Complex.i).IsZero) Then
                                    bIsImag = True
                                ElseIf vFact(j).IsPolynomial Then
                                    Dim vPa() As Polynomial = vFact(j).getPolynomial.splitIntoTerms
                                    Dim Pa As Polynomial = vFact(j).getPolynomial
                                    Dim imag As New Polynomial(0.0)
                                    For k As Int64 = 0 To vPa.Length - 1
                                        For ic As Int64 = 0 To vPa(k).cf.Length - 1
                                            vPa(k).cf(ic).pRe = New Rational(0.0)
                                        Next
                                        imag += vPa(k)
                                    Next

                                    vFact(j) = New Expression(imag)
                                    If Pa.PolyResto IsNot Nothing Then
                                        vPa = Pa.PolyResto.splitIntoTerms
                                        Dim imagNum As New Polynomial(0.0)
                                        For k As Int64 = 0 To vPa.Length - 1
                                            For ic As Int64 = 0 To vPa(k).cf.Length - 1
                                                vPa(k).cf(ic).pRe = New Rational(0.0)
                                            Next
                                            imagNum += vPa(k)
                                        Next
                                        vFact(j) += New Expression(imagNum / Pa.PolyDivisor)
                                    End If
                                    bIsImag = True
                                End If
                            Next
                            If Not bIsImag Then
                            Else
                                vSum(i) = Expression.factorsToExpr(vFact, bIsDiv)
                                vSum(i) = rup.ReduceUsingPolynomials(vSum(i))
                                eMc.getExpr(row, col) += vSum(i)
                            End If
                        Next
                    End If
                Next
            Next
            Dim ieMtx As New ExprMatrix(Complex.i)
            eMc /= ieMtx
            eMc.cfg = cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Function
    Public Function opDerivative(ByVal sVarResp As String) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(Me.cfg, 0, 0)
            Dim col As Int64
            For row As Int64 = 0 To Me.Rows - 1
                For col = 0 To Me.expr(row).Length - 1
                    Dim expr As Expression = Me.getExpr(row, col)
                    If expr IsNot Nothing Then
                        eMc.AddRowCol(row, col)
                        eMc.getExpr(row, col) = _
                            expr.opDeriv(sVarResp)
                    End If
                Next
            Next
            eMc.cfg = cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Function
    Public Function opIntegral( _
                               ByVal cfg As Config, _
                               ByVal respVarID As Int64, _
                               ByVal vExprParams() As Expression, _
                               ByVal cur As currentMatch, _
                               ByVal vars As VarsAndFns, _
                                ByVal IRespVar As String, _
                               ByRef origAntideriv() As Expression, _
                               ByRef bFoundOrigAntideriv() As Boolean, _
                               ByRef vIndex() As Int64 _
                    ) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(Me.cfg, 0, 0)
            ReDim vExprParams(0)
            Dim col As Int64
            For row As Int64 = 0 To Me.Rows - 1
                For col = 0 To Me.expr(row).Length - 1
                    Dim expr As Expression = Me.getExpr(row, col)
                    If expr IsNot Nothing Then
                        eMc.AddRowCol(row, col)
                        eMc.getExpr(row, col) = _
                            expr.opAntiDerivative( _
                                cfg, respVarID, vExprParams, cur, vars, IRespVar, Nothing, Nothing, Nothing)
                    End If
                Next
            Next
            'eMc.cfg = cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Function
    Public Function opIntegralDefOrPolynFraction( _
                           ByVal respVarID As Int64, _
                           ByVal vExprParams() As Expression, _
                           ByVal cur As currentMatch, _
                           ByVal vars As VarsAndFns) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(cfg, Me.Rows, Me.getExprAtRow(0).Length - 2)
            Dim col As Int64
            ' The first two entries, in row=0 and column= 0 and column=1 are supposed to
            ' hold the lower and upper limit for the definite integral:
            Dim currvExprParam() As Expression = _
                New Expression() { _
              Me.getExpr(0, 0), Me.getExpr(0, 1), Nothing}
            ' currvExprParam(0) = lower limit
            '      "        (1) = upper limit
            eMc = New ExprMatrix(Me.cfg, 0, 0)
            ReDim vExprParams(0)
            ' ... all the other entries will be integrated:
            For row As Int64 = 0 To Me.Rows - 1
                For col = 0 To Me.expr(row).Length - 1
                    Dim col2 As Int64 = col
                    If row = 0 Then col2 = col + 2
                    If col2 >= Me.expr(row).Length Then
                        Exit For
                    End If
                    Dim expr As Expression = Me.getExpr(row, col2)
                    If expr IsNot Nothing Then
                        currvExprParam(2) = expr
                        eMc.AddRowCol(row, col)
                        ' obtain the definite integral:
                        eMc.getExpr(row, col) = _
                            Expression.opIntegralDefOrPolinFraction( _
                                cfg, respVarID, currvExprParam, cur, vars)
                    End If
                Next
            Next
            eMc.cfg = cfg
        Catch ex As Exception
            Throw ex
        End Try
        Return eMc
    End Function
    Public Function opExponential() As ExprMatrix
        Dim rMtx As ExprMatrix = Nothing
        Try
            rMtx = New ExprMatrix(Me.cfg, Me.Rows, Me.Cols)

            Dim Identity As New ExprMatrix(Me.cfg, Me.Rows, Me.Cols)
            Dim i, j As Int64
            For i = 0 To Identity.Rows - 1
                For j = 0 To Identity.Cols - 1
                    rMtx.getExpr(i, j) = New Expression(0.0)
                    If i <> j Then
                        Identity.getExpr(i, j) = New Expression(0.0)
                    Else
                        Identity.getExpr(i, j) = New Expression(1.0)
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
                Term *= Me / New Expression(k)
                k += 1D
            Loop While k < 27 ' enough?
        Catch ex As Exception
            Throw ex
        End Try
        Return rMtx
    End Function

    Public Function RowsToArrayCjo() As Complex()
        Dim cjo(Me.getMatrix.vVect.Length - 1) As Complex
        Try
            For i As Int64 = 0 To cjo.Length - 1
                cjo(i) = New Complex(Me.getMatrix.vVect(i).vPoly(0).cf(0))
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return cjo
    End Function
    Public Function toDouble() As Double
        Return expr(0)(0).toDouble
    End Function
    Public Function toComplex() As Complex
        Return expr(0)(0).toComplex
    End Function
    Public Function ToStrExprMtx(cfg As Config) As String
        Dim e1(expr.Length - 1) As String
        Try
            For i As Int64 = 0 To expr.Length - 1
                Dim e2(expr(i).Length - 1) As String
                For j As Int64 = 0 To expr(i).Length - 1
                    If expr(i)(j) Is Nothing Then
                        e2(j) = ""
                    Else
                        e2(j) = expr(i)(j).ToStringExpr(cfg)
                    End If
                Next
                If cfg.bAltMtxOutput Then
                    ' Is there more than one row or column?
                    If expr(i).Length > 1 OrElse expr.Length > 1 Then
                        e1(i) = "[" + Join(e2, ",") + "]"
                    Else
                        e1(i) = Join(e2, ",")
                    End If
                Else
                    e1(i) = Join(e2, ";")
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
        If cfg.bAltMtxOutput AndAlso expr.Length > 1 Then
            Return "[" + Join(e1, "") + "]"
        End If
        Return Join(e1, vbCrLf)
    End Function
    Public ReadOnly Property ToStringExprMtx(cfg As Config) As String
        Get
            Return ToStrExprMtx(cfg)
        End Get
    End Property
    Public Overrides Function ToString() As String
        Return ToStrExprMtx(Config.cfg)
    End Function
    Public Function ToStringInfo(cfg As Config) As String
        Dim e1(expr.Length - 1) As String
        Try
            For i As Int64 = 0 To expr.Length - 1
                Dim e2(expr(i).Length - 1) As String
                For j As Int64 = 0 To expr(i).Length - 1
                    If expr(i)(j).IsPolynomial Then
                        e2(j) = expr(i)(j).getPolynomial.ToStringInfo(cfg, expr(i)(j).getPolynomial.roots)
                    End If
                Next
                e1(i) = Join(e2, ";")
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return Join(e1, vbCrLf)
    End Function
    Public Shared Function tryParseExprMatrix(ByVal Input As String,
                                    ByRef result As ExprMatrix,
                                    Optional ByVal cfg As Config = Nothing) As Boolean
        Static rup As New ReduceExprUsingPolynomials
        Try
            If cfg Is Nothing Then
                cfg = New Config
                cfg.timeOutms = 120000
                cfg.doTimeOut = timeout.whenTimeIsOver
                cfg.resetStartTime()
                Config.reset(cfg)
            End If
            Input = Replace(Input, " ", "")
            Dim sMtxOp As String = "(\r|\n|\||" + MathGlobal8.sRP + ")(?<mtxOp>\" +
             Join(MathGlobal8.vOp, "|\") + ")(\r|\n|\||" + MathGlobal8.sLP + ")"
            Dim mc As MatchCollection = Regex.Matches(Input, sMtxOp)
            Dim i As Int32
            Dim vInput(-1) As Match, iv As Int32 = 0
            Dim pos As Int32 = 0
            For i = 0 To mc.Count
                Dim vRow() As String
                If i < mc.Count Then
                    Dim ln As Int32 = mc(i).Index - pos
                    If i = 0 Then
                        vRow = Regex.Split(Input.Substring(pos + 1, ln - 1), MathGlobal8.sRow)
                    Else
                        vRow = Regex.Split(Input.Substring(pos, ln), MathGlobal8.sRow)
                    End If
                    pos += ln + mc(i).Length
                Else
                    If mc.Count = 0 Then
                        ReDim Preserve vInput(iv)
                        vInput(iv) = Regex.Match(Input.Substring(pos), "(?<expr>.+)")
                        iv += 1
                        Exit For
                    Else
                        vRow = New String() {Input.Substring(pos, Input.Length - pos - 1)}
                    End If
                End If
                Dim row As Int32
                For row = 0 To vRow.Length - 1
                    ReDim Preserve vInput(iv)
                    vInput(iv) = Regex.Match("[" + vRow(row) + "]", "(?<expr>.+)")
                    iv += 1
                Next
                If i < mc.Count Then
                    ReDim Preserve vInput(iv)
                    vInput(iv) = MathGlobal8.CloneMatch(mc(i))
                    iv += 1
                End If
            Next
            Dim toRPN As New m8ToRPN
            toRPN.input = vInput
            Dim vTkn() As Token = toRPN.M8ToRPN()
            Dim rpn As New RPN_Stack
            rpn.oStack = vTkn
            rpn.iSt = vTkn.Length
            Dim t As Token = Nothing
            rpn.Eval(t)
            result = t.dblVal
        Catch ex As Exception
            Return False
        End Try
        Return True
    End Function
End Class
