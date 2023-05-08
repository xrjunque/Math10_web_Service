Imports System.Text.RegularExpressions

Public Class Matrix

    Public Sub New()
        vVect = New Vector() {New Vector(0.0)}
    End Sub

    Public vVect() As Vector
    Dim dbMod As Double = 0.0
    Dim vars As VarsAndFns
    Public Function getAllVars() As String()
        Dim eMtx As New ExprMatrix(Me)
        Return eMtx.getAllVars
    End Function
    Public Sub setVars(ByVal vars As VarsAndFns)
        Me.vars = vars
    End Sub
    Public Sub New(ByVal Re As Double)
        vVect = New Vector() {New Vector(Re)}
    End Sub
    Public Sub New(ByVal Re As Double, ByVal Im As Double)
        vVect = New Vector() {New Vector(Re, Im)}
    End Sub
    Public Sub New(ByVal cjo As Complex)
        vVect = New Vector() {New Vector(cjo)}
    End Sub
    Public Sub New(ByVal polyA As Polynomial)
        vVect = New Vector() {New Vector(polyA)}
    End Sub
    Public Sub New(ByVal Va As Vector)
        vVect = New Vector() {Va}
    End Sub
    Public Sub New(ByVal Ma As Matrix)
        Try
            ReDim vVect(Ma.vVect.Length - 1)
            Dim i As Int64
            For i = 0 To Ma.vVect.Length - 1
                vVect(i) = New Vector(Ma.vVect(i))
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
    End Sub

    Public Property dbModuloInOperations() As Double
        Get
            Return dbMod
        End Get
        Set(ByVal value As Double)
            dbMod = value
            Dim i As Int64
            For i = 0 To Me.vVect.Length - 1
                vVect(i).dbModuloInOperations = dbMod
            Next
        End Set
    End Property

    Public Overloads Shared Operator -(ByVal Ma As Matrix) As Matrix
        Dim Mc As New Matrix(0)
        Dim i As Int64
        Try
            ReDim Mc.vVect(Ma.vVect.Length - 1)
            For i = 0 To Mc.vVect.Length - 1
                Mc.vVect(i) = -Ma.vVect(i)
            Next
        Catch ex As Exception
            Throw New Exception(msg8.msg(1003)) ' unknown error
        End Try
        Return Mc
    End Operator
    Public Overloads Shared Operator -(ByVal Ma As Matrix, ByVal Mb As Matrix) As Matrix
        Dim Mc As Matrix
        Dim i As Int64
        Try
            Dim tipoA As mtxTipo = Ma.mtxType
            Dim tipoB As mtxTipo = Mb.mtxType
            If tipoA <= mtxTipo.complex AndAlso tipoB <= mtxTipo.complex Then
                Return New Matrix(Ma.vVect(0).vPoly(0).cf(0) - _
                                  Mb.vVect(0).vPoly(0).cf(0))
            End If
            Mc = New Matrix()
            If Ma.vVect.Length = Mb.vVect.Length Then
                ReDim Preserve Mc.vVect(Ma.vVect.Length - 1)
                For i = 0 To Ma.vVect.Length - 1
                    Mc.vVect(i) = Ma.vVect(i) - Mb.vVect(i)
                Next
            Else
                Throw New Exception(msg8.msg(1019))
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Mc
    End Operator
    Public Overloads Shared Operator +(ByVal Ma As Matrix, ByVal Mb As Matrix) As Matrix
        Dim Mc As Matrix
        Dim i As Int64
        Try
            Dim tipoA As mtxTipo = Ma.mtxType
            Dim tipoB As mtxTipo = Mb.mtxType
            If tipoA <= mtxTipo.complex AndAlso tipoB <= mtxTipo.complex Then
                Return New Matrix(Ma.vVect(0).vPoly(0).cf(0) + _
                                  Mb.vVect(0).vPoly(0).cf(0))
            End If
            Mc = New Matrix()
            If Ma.vVect.Length = Mb.vVect.Length Then
                ReDim Preserve Mc.vVect(Ma.vVect.Length - 1)
                For i = 0 To Ma.vVect.Length - 1
                    Mc.vVect(i) = Ma.vVect(i) + Mb.vVect(i)
                Next
            Else
                Throw New Exception(msg8.msg(1019))
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Mc
    End Operator
    Public Shared Function opTranspose(ByVal Ma As Matrix) As Matrix
        Dim Mc As New Matrix(0)
        Try
            Dim i As Int64
            ReDim Mc.vVect(Ma.vVect(0).vPoly.Length - 1)
            For iCol As Int64 = 0 To Ma.vVect(0).vPoly.Length - 1
                Mc.vVect(iCol) = New Vector
                ReDim Preserve Mc.vVect(iCol).vPoly(Ma.vVect.Length - 1)
                For i = 0 To Ma.vVect.Length - 1
                    Mc.vVect(iCol).vPoly(i) = New Polynomial(Ma.vVect(i).vPoly(iCol))
                Next
            Next
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Mc
    End Function
    Public Overloads Shared Operator *(ByVal Ma As Matrix, ByVal Mb As Matrix) As Matrix
        Dim Mc As New Matrix
        Dim i, j As Int64
        Try
            Dim tipoA As mtxTipo = Ma.mtxType
            Dim tipoB As mtxTipo = Mb.mtxType
            If tipoA <= mtxTipo.complex AndAlso tipoB <= mtxTipo.complex Then
                Return New Matrix(Ma.vVect(0).vPoly(0).cf(0) * _
                                  Mb.vVect(0).vPoly(0).cf(0))
            ElseIf tipoA <= mtxTipo.polynomial AndAlso tipoB <= mtxTipo.polynomial Then
                Return New Matrix(Ma.vVect(0).vPoly(0) * _
                                  Mb.vVect(0).vPoly(0))
            End If
            Dim MbT As Matrix = Matrix.opTranspose(Mb)
            ReDim Mc.vVect(Ma.vVect.Length - 1)
            ' Ma(n x m) * Mb (m * k) --> Mc (n * k)
            If Ma.vVect(0).vPoly.Length = Mb.vVect.Length Then
                For i = 0 To Ma.vVect.Length - 1
                    ReDim Preserve Mc.vVect(i)
                    Mc.vVect(i) = New Vector
                    ReDim Mc.vVect(i).vPoly(Mb.vVect(0).vPoly.Length - 1)
                    For j = 0 To Mb.vVect(0).vPoly.Length - 1
                        Mc.vVect(i).vPoly(j) = (Ma.vVect(i) * MbT.vVect(j)).vPoly(0)
                    Next
                Next
            Else
                Throw New Exception(msg8.msg(1019)) ' Lenghts don't match
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Mc
    End Operator
    Public Overloads Shared Operator /(ByVal Ma As Matrix, ByVal Mb As Matrix) As Matrix
        Dim Mc As Matrix
        Dim tipoA As mtxTipo = Ma.mtxType
        Dim tipoB As mtxTipo = Mb.mtxType
        If tipoA <= mtxTipo.complex AndAlso tipoB <= mtxTipo.complex Then
            Mc = New Matrix(Ma.vVect(0).vPoly(0).cf(0) / _
                              Mb.vVect(0).vPoly(0).cf(0))
        ElseIf tipoA <= mtxTipo.polynomial AndAlso tipoB <= mtxTipo.polynomial Then
            Mc = New Matrix(Ma.vVect(0).vPoly(0) / _
                              Mb.vVect(0).vPoly(0))
        Else
            Mc = Ma * (Mb ^ New Matrix(-1))
        End If
        Return Mc
    End Operator
    Public Shared Function opMod(ByVal Ma As Matrix, ByVal Mb As Matrix) As Matrix
        Dim Mc As Matrix
        Dim tipoA As mtxTipo = Ma.mtxType
        Dim tipoB As mtxTipo = Mb.mtxType
        If tipoA <= mtxTipo.complex AndAlso tipoB <= mtxTipo.complex Then
            Mc = New Matrix(New Complex(Ma.vVect(0).vPoly(0).cf(0).pRe.ToDouble Mod _
                                      Mb.vVect(0).vPoly(0).cf(0).pRe.ToDouble))
        ElseIf tipoA <= mtxTipo.polynomial AndAlso tipoB <= mtxTipo.polynomial Then
            Ma.vVect(0).vPoly(0).dbModuloInOperations = Mb.vVect(0).vPoly(0).cf(0).pRe.ToDouble
            Mc = New Matrix(Ma.vVect(0).vPoly(0))
        Else
            Throw New Exception(msg8.num(9))
        End If
        Return Mc
    End Function
    Public Shared Function opIntegral(ByVal Ma As Matrix) As Matrix
        Dim Mc As New Matrix(Ma)
        Dim i, j As Int64
        For i = 0 To Mc.vVect.Length - 1
            For j = 0 To Mc.vVect(i).vPoly.Length - 1
                Mc.vVect(i).vPoly(j) = Mc.vVect(i).vPoly(j).opIntegral
            Next
        Next
        Return Mc
    End Function
    Public Shared Function opOrthog(ByVal Ma As Matrix) As Matrix
        Dim Mc As New Matrix
        Try
            Mc = New Matrix(Polynomial.opOrthog(Ma))
        Catch ex As Exception

        End Try
        Return Mc
    End Function
    Public Overloads Shared Operator ^(ByVal Ma As Matrix, ByVal iExp As Int64) As Matrix
        Dim Mc As New Matrix(Ma)
        Try
            ' Exponent is integer
            Select Case iExp
                Case 0 : Throw New Exception(msg8.msg(1022)) ' Null exponent
                Case 1 : Exit Try
                Case Is > 1
                    Dim i As Int64
                    For i = 2 To iExp
                        Mc *= Ma
                    Next
                Case Is < 0
                    Dim i As Int64
                    For i = 2 To -iExp
                        Mc *= Ma
                    Next
                    ' Obtain Inverse(Mc):
                    ' 'Mc = Functions.Inverse_Matrix_GaussPivotTotal(Mc)
                    'Mc = Functions.Inverse_Matrix_GaussPivotTotal_poly(Mc)
                    If Mc.vVect.Length = 1 AndAlso
                    Mc.vVect(0).vPoly.Length = 1 Then
                        Mc = New Matrix(New Polynomial(1.0) / Mc.vVect(0).vPoly(0))
                    Else
                        'Mc = Functions.Inverse_Matrix_GaussPivotTotal_poly(Mc, "")
                        Matrix.TryParseMatrix((New ExprMatrix(Mc)) ^ New ExprMatrix(-1), Mc)
                    End If
            End Select

        Catch ex As Exception
            Throw ex
        End Try
        Return Mc
    End Operator
    Public Overloads Shared Operator ^(ByVal Ma As Matrix, ByVal Mb As Matrix) As Matrix
        Dim Mc As Matrix
        Try
            Dim tipoA As mtxTipo = Ma.mtxType
            Dim tipoB As mtxTipo = Mb.mtxType
            If tipoA <= mtxTipo.complex AndAlso tipoB <= mtxTipo.complex Then
                Dim cjo As Complex = Ma.vVect(0).vPoly(0).cf(0) ^ _
                                  Mb.vVect(0).vPoly(0).cf(0)
                Return New Matrix(cjo)
            ElseIf tipoA = mtxTipo.polynomial AndAlso tipoB <= mtxTipo.complex Then
                Dim polyC As Polynomial = Ma.vVect(0).vPoly(0) ^ _
                                  Mb.vVect(0).vPoly(0)
                Return New Matrix(polyC)
            End If
            If Mb.vVect.Length = 1 AndAlso Mb.vVect(0).vPoly.Length = 1 AndAlso _
                 Mb.mtxType = mtxTipo.real Then
                Mc = New Matrix(Ma)
                Dim expDb As Double = Mb.vVect(0).vPoly(0).cf(0).pRe.ToDouble
                Dim exp As Int64 = expDb
                If exp = Math.Floor(expDb) Then
                    Return Ma ^ exp
                    '' Exponent is integer
                    'Select Case exp
                    '    Case 0 : Throw New Exception(msg8.msg(1022)) ' Null exponent
                    '    Case 1 : Exit Try
                    '    Case Is > 1
                    '        Dim i As Int64
                    '        For i = 2 To exp
                    '            Mc *= Ma
                    '        Next
                    '    Case Is < 0
                    '        Dim i As Int64
                    '        For i = 2 To -exp
                    '            Mc *= Ma
                    '        Next
                    '        ' Obtain Inverse(Mc):
                    '        ' 'Mc = Functions.Inverse_Matrix_GaussPivotTotal(Mc)
                    '        'Mc = Functions.Inverse_Matrix_GaussPivotTotal_poly(Mc)
                    '        Mc = Functions.Inverse_Matrix_GaussPivotTotal_poly(Mc)
                    'End Select
                Else
                    Throw New Exception(msg8.msg(1021)) ' Exponent non integer 
                End If
            Else
                Throw New Exception(msg8.msg(1021))
            End If
        Catch ex As Exception
            Throw New Exception(ex.Message)
        End Try
        Return Mc
    End Operator
    Public Shared Function opEchelonForm(ByVal Ma As Matrix) As Matrix
        'Dim Mc As Matrix = Nothing
        'Dim msg As String = String.Empty
        'Try
        '    Mc = Functions.Inverse_Matrix_GaussPivotTotal_poly(Ma, msg, True)
        '    If Len(msg) Then
        '        Throw New Exception(msg)
        '    ElseIf Mc Is Nothing Then
        '        Throw New Exception(msg8.num(13)) ' n/a
        '    End If
        'Catch ex As Exception
        '    Throw ex
        'End Try
        'Return Mc
        Dim A As New Matrix(Ma)
        Dim B As New Matrix(Ma)
        Try
            Dim i, j As Int32
            Dim maxI, maxJ As Int32, maxNorm As Double = 0.0
            For i = 0 To B.vVect.Length - 1
                For j = 0 To B.vVect(i).vPoly.Length - 1
                    If i = j Then
                        B.vVect(i).vPoly(j) = New Polynomial(1.0)
                    Else
                        B.vVect(i).vPoly(j) = New Polynomial(0.0)
                    End If
                Next
            Next
            Dim minRowCol As Int32 = Math.Min(A.vVect.Length, A.vVect(0).vPoly.Length - 1)
            For iDone As Int32 = 0 To minRowCol - 1
                maxNorm = 0.0
                For i = iDone To A.vVect.Length - 1
                    'Dim fin As Int32 = IIf(bDoInverseMatrix, A.vVect(i).vPoly.Length - 1, iDone)
                    For j = iDone To iDone
                        If A.vVect(i).vPoly(j).isComplex Then
                            Dim db As Double = A.vVect(i).vPoly(j).ToComplex.opNorm
                            If db > maxNorm Then
                                maxNorm = db
                                maxI = i : maxJ = j
                            End If
                        End If
                    Next
                Next
                If maxI <> iDone Then
                    A.RowChange(iDone, maxI)
                    B.RowChange(iDone, maxI)
                End If
                'If bDoInverseMatrix Then
                '    If maxJ <> iDone Then
                '        A.ColChange(iDone, maxJ)
                '        B.ColChange(iDone, maxJ)
                '    End If
                'End If


                Dim divBy As New Polynomial(-A.vVect(iDone).vPoly(iDone))
                For i = iDone + 1 To A.vVect.Length - 1
                    Dim multBy As New Polynomial(A.vVect(i).vPoly(iDone))
                    If Not (divBy.isComplex AndAlso divBy.ToComplex.opNorm = 0.0) Then
                        Dim r As Rational
                        If multBy.isReal AndAlso divBy.isReal Then
                            r = New Rational(multBy.ToDouble, divBy.ToDouble)
                            Dim p As New Polynomial(New Complex(r, New Rational(0.0)))
                            A.RowI_plus_RowJ_times_alpha(i, iDone, p)
                            B.RowI_plus_RowJ_times_alpha(i, iDone, p)
                        Else
                            A.RowI_plus_RowJ_times_alpha(i, iDone, multBy / divBy)
                            B.RowI_plus_RowJ_times_alpha(i, iDone, multBy / divBy)
                        End If
                    End If
                Next
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return A
    End Function
    Public Shared Function opDeterminant(ByVal Ma As Matrix) As Polynomial
        Dim Pc As New Polynomial(1.0)
        Dim msg As String = String.Empty
        Try
            ' Set last param = True and return
            ' value will be an equivalent lower  
            ' triangular matrix:
            'Dim Mc As Matrix = _
            'Functions.Inverse_Matrix_GaussPivotTotal_poly(Ma, msg)
            Dim Mc As New Matrix
            Matrix.TryParseMatrix((New ExprMatrix(Ma)) ^ New ExprMatrix(-1), Mc)
            If Len(msg) Then
                Throw New Exception(msg)
            ElseIf Mc Is Nothing Then
                Throw New Exception(msg8.num(13)) ' n/a
            End If
            Dim i As Int64
            ' The determinant of a triangular matrix equals 
            ' the product of the diagonal entries. See:
            ' http://en.wikipedia.org/wiki/Triangular_matrix#Special_properties
            For i = 0 To Mc.vVect.Length - 1
                Pc *= Mc.vVect(i).vPoly(i)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return Pc
    End Function


    Public Shared Function TryParseMatrix(ByVal eM As ExprMatrix, ByRef result As Matrix) As Boolean
        Try
            Dim i, j As Int64
            result = New Matrix
            ReDim Preserve result.vVect(eM.Rows - 1)
            Dim index1(-1) As Int64
            For i = 0 To eM.Rows - 1
                result.vVect(i) = New Vector(0.0)
                ReDim Preserve result.vVect(i).vPoly(eM.Cols - 1)
                For j = 0 To eM.Cols - 1
                    If Not eM.getExpr(i, j).IsPolynomial Then
                        Return False
                    Else
                        result.vVect(i).vPoly(j) = eM.getExpr(i, j).getPolynomial
                    End If
                Next
            Next
            Return True
        Catch ex As Exception
            Throw ex
        End Try
        Return False
    End Function
    Public Sub setRowAndCol(ByVal nrows As Int64, ByVal ncols As Int64)
        Dim i, i1 As Int64
        If nrows >= vVect.Length Then
            i1 = vVect.Length
            ReDim Preserve vVect(nrows)
            For i = i1 To nrows
                vVect(i) = New Vector(0.0)
            Next
        End If
        If ncols >= vVect(nrows).vPoly.Length Then
            For row = 0 To nrows
                i1 = vVect(row).vPoly.Length
                ReDim Preserve vVect(row).vPoly(ncols)
                For i = i1 To ncols
                    vVect(row).vPoly(i) = New Polynomial(0.0)
                Next
            Next
        End If
    End Sub
    Public Sub maxZeroRowOrCol(ByRef row As Int64,
                                  ByRef col As Int64)
        Try
            ' On exit, if row<>-1 (or col<>-1) represents
            ' the matrix's row (or column) with maximum number 
            ' of zeros in its entries of any row/column.
            row = -1
            col = -1
            Dim nMax As Int64
            Dim i, j As Int64
            Dim rows As Int64 = Me.vVect.Length
            Dim cols As Int64 = Me.vVect(0).vPoly.Length
            For i = 0 To rows - 1
                Dim iZeros As Int64 = 0
                For j = 0 To cols - 1
                    If vVect(i).vPoly(j).isComplex AndAlso
                    vVect(i).vPoly(j).ToComplex.IsZero Then
                        iZeros += 1
                    End If
                Next
                If iZeros > nMax Then
                    nMax = iZeros
                    row = i ' save's the index
                End If
            Next
            For j = 0 To cols - 1
                Dim iZeros As Int64 = 0
                For i = 0 To rows - 1
                    If vVect(i).vPoly(j).isComplex AndAlso
                    vVect(i).vPoly(j).ToComplex.IsZero Then
                        iZeros += 1
                    End If
                Next
                If iZeros > nMax Then
                    nMax = iZeros
                    col = j ' save's the index
                    row = -1
                End If
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub RowChange(changeRowA As Int32, byRowB As Int32)
        Try
            Dim vA As New Vector(Me.vVect(changeRowA))
            Dim vB As New Vector(Me.vVect(byRowB))
            Me.vVect(changeRowA) = vB
            Me.vVect(byRowB) = vA
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub ColChange(changeColA As Int32, byColB As Int32)
        Try
            For row As Int32 = 0 To Me.vVect.Length - 1
                Dim pA As New Polynomial(Me.vVect(row).vPoly(changeColA))
                Me.vVect(row).vPoly(changeColA) = Me.vVect(row).vPoly(byColB)
                Me.vVect(row).vPoly(byColB) = pA
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub RowMultiplyBy(iRow As Int32, ByPoly As Polynomial)
        vVect(iRow) *= ByPoly
    End Sub
    Public Sub ColMultiplyBy(iCol As Int32, ByPoly As Polynomial)
        Try
            For row As Int32 = 0 To Me.vVect.Length - 1
                Me.vVect(row).vPoly(iCol) *= ByPoly
            Next
        Catch ex As Exception
            Throw ex
        End Try
    End Sub
    Public Sub RowI_plus_RowJ_times_alpha(rowI As Int32, rowJ As Int32, alpha As Polynomial)
        Try
            Dim A As New Matrix(Me)
            A.RowMultiplyBy(rowJ, alpha)
            For col As Int32 = 0 To vVect(rowI).vPoly.Length - 1
                Me.vVect(rowI).vPoly(col) += A.vVect(rowJ).vPoly(col)
            Next
        Catch ex As Exception

        End Try
    End Sub
    Public Sub ColI_plus_CojJ_times_alpha(colI As Int32, colJ As Int32, alpha As Polynomial)
        Try
            Dim a As New Matrix(Me)
            a.ColMultiplyBy(colJ, alpha)
            For row As Int32 = 0 To vVect.Length - 1
                Me.vVect(row).vPoly(colI) += a.vVect(row).vPoly(colJ)
            Next
        Catch ex As Exception

        End Try
    End Sub
    Public ReadOnly Property mtxType() As mtxTipo
        Get
            If vVect.Length = 0 Then
                Return mtxTipo.empty
            End If
            If vVect.Length = 1 Then
                If vVect(0).vPoly.Length = 1 Then
                    If vVect(0).vPoly(0).isComplex Then
                        If vVect(0).vPoly(0).cf(0).pIm.IsZero Then
                            Return mtxTipo.real
                        Else
                            Return mtxTipo.complex
                        End If
                    Else
                        Return mtxTipo.polynomial
                    End If
                Else
                    Return mtxTipo.vector
                End If
            Else
                Return mtxTipo.matrix
            End If
        End Get
    End Property

    Public ReadOnly Property Var() As String()
        Get
            Dim iVars(-1) As String
            Dim i As Int64
            If vVect.Length = 0 Then
                Return iVars
            End If
            iVars = vVect(0).Var
            For i = 1 To vVect.Length - 1
                Dim vCur() As String = vVect(i).Var
                If iVars.Length = 0 Then
                    iVars = vCur
                ElseIf vCur.Length Then
                    Dim j As Int64
                    For j = 0 To vCur.Length - 1
                        Dim pos As Int64 = Array.IndexOf(iVars, vCur(j))
                        If pos = -1 Then
                            ReDim Preserve iVars(iVars.Length)
                            iVars(iVars.Length - 1) = vCur(j)
                        End If
                    Next
                End If
            Next
            Return iVars
        End Get
    End Property
    Public Function toStringRoots(cfg As Config) As String()
        ' Converts a roots vector into a string()  ( (-1,1)--> Real solutions: Root1: -1 Root2: 1 )
        Dim e1(-1) As String, ie As Int64 = 0
        Dim i As Int64
        Dim bRe As Boolean = False, bIm As Boolean = False
        'Dim msg As New Messages(nId)
        'Dim e3() As String = Split(Me.ToString, ",")
        Dim sPre As String = "<font color=""red"">"
        Dim sPost As String = "</font>"
        For i = 0 To Me.vVect.Length - 1
            Dim reAbs As Double = Math.Round(Math.Abs(vVect(i).vPoly(0).cf(0).pRe.ToDouble), 3)
            Dim imAbs As Double = Math.Round(Math.Abs(vVect(i).vPoly(0).cf(0).pIm.ToDouble), 3)
            Dim e2 As String = sPre + vVect(i).vPoly(0).toStringPoly(cfg) + sPost
            If cfg.bRounding Then
                If reAbs + imAbs = 0 Then
                ElseIf reAbs = 0 Then
                    Dim cjo As New Complex(New Rational(0.0), _
                                           vVect(i).vPoly(0).cf(0).pIm)
                    e2 = sPre + cjo.toStringComplex(cfg) + sPost
                ElseIf imAbs = 0 Then
                    Dim cjo As New Complex(vVect(i).vPoly(0).cf(0).pRe, New Rational(0.0))
                    e2 = sPre + cjo.toStringComplex(cfg) + sPost
                End If
            End If
            If InStr(e2, "i") = 0 Then
                If bRe = False Then
                    ReDim Preserve e1(ie)
                    e1(ie) = String.Format(msg8.msg(20, Nothing, False)) ' real solutions
                    ie += 1
                    bRe = True
                End If
                ReDim Preserve e1(ie)
                e1(ie) = String.Format(msg8.msg(21, Nothing, False), i + 1) + " " + e2
                ie += 1
            Else
                If bIm = False Then
                    ReDim Preserve e1(ie)
                    e1(ie) = String.Format(msg8.msg(25, Nothing, False)) ' complex roots
                    ie += 1
                    bIm = True
                    bRe = True
                End If
                ReDim Preserve e1(ie)
                e1(ie) = String.Format(msg8.msg(21, Nothing, False), i + 1) + " " + e2
                ie += 1
            End If
        Next
        Return e1
    End Function
    Public Function tovCjo() As Complex()
        Dim cjo(vVect.Length - 1) As Complex
        Try
            Dim i As Int64
            For i = 0 To cjo.Length - 1
                cjo(i) = vVect(i).vPoly(0).cf(0)
            Next
        Catch ex As Exception
            Throw ex
        End Try
        Return cjo
    End Function
    Public ReadOnly Property toStringPoly(cfg As Config)
        Get
            Return toStringMtx(cfg)
        End Get
    End Property
    Public Function toStringInfo(cfg As Config) As String
        Dim sInfo As String = ""
        Try
            If Me.vVect.Length = 1 AndAlso _
            Me.vVect(0).IsPolynomial Then
                sInfo = Me.vVect(0).vPoly(0).ToStringInfo(cfg)
            End If
        Catch ex As Exception
            Throw ex
        End Try
        Return sInfo
    End Function
    Public Overloads Function ToString() As String
        Return toStringMtx(New Config)
    End Function
    Public Overloads Function ToString(cfg As Config) As String
        Return toStringMtx(cfg)
    End Function
    Public ReadOnly Property toStringMtx( _
                cfg As Config) As String
        Get
            Dim e1(vVect.Length - 1) As String
            Try
                Dim i As Int64
                For i = 0 To vVect.Length - 1
                    e1(i) = vVect(i).toStrVect(cfg)
                Next
            Catch ex As Exception
                Throw New Exception(ex.Message)
            End Try
            Return Join(e1, vbCrLf)
        End Get
    End Property
    Public Function toStringMtxHTML(cfg As Config) As String
        Dim t As String = "<TABLE BORDER=""1"" borderColor=""#d0d0d0"">"
        Try
            Dim i, j As Int64
            Dim e1 As String = toStringMtx(cfg)
            e1 = Replace(e1, "|", vbCrLf)
            Dim vRow() As String = Split(e1, vbCrLf)
            For i = 0 To vRow.Length - 1
                vRow(i) = Replace(vRow(i), vbTab, ";")
                Dim vCol() As String = Split(vRow(i), ";")
                t += "<TR borderColor=""#d0d0d0"" ALIGN=""CENTER"">"
                For j = 0 To vCol.Length - 1
                    t += "<TD>" + vCol(j) + "</TD>"
                Next
                t += "</TR>"
            Next
            t += "</TABLE>"
        Catch ex As Exception
        End Try
        Return t
    End Function
End Class
Public Enum mtxTipo As Integer
    empty = 1
    real = 2
    complex = 4
    polynomial = 8
    polyMultiVar = 16
    vector = 32
    matrix = 64
End Enum
