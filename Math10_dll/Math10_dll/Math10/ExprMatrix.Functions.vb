'Imports System.Windows.Forms

Partial Public Class ExprMatrix

    ' Gaussian elimination algorithm:
    Public Shared Function GaussianElimination(
    eM As ExprMatrix, IsAumentedMatrix As Boolean, ByRef vVars() As String, FindInversMatrix As Boolean, ByRef invMtx As ExprMatrix) As ExprMatrix
        Dim rMtx As ExprMatrix
        Dim bmathml As Boolean = G10.mathml
        G10.mathml = False
        Try
            invMtx = New ExprMatrix
            Dim i As Int32
            rMtx = New ExprMatrix(eM)
            If FindInversMatrix Then
                IsAumentedMatrix = False
                invMtx = New ExprMatrix
                For i = 0 To eM.Rows - 1
                    Dim vExpr(eM.Columns(i) - 1) As Expression
                    For j As Int32 = 0 To vExpr.Length - 1
                        If i <> j Then
                            vExpr(j) = New Expression(0.0)
                        Else
                            vExpr(j) = New Expression(1.0)
                        End If
                    Next
                    invMtx.AddRow(vExpr)
                Next
            End If
            ' The steps are as follows
            '1) Start from k=0 And l=0.
            '2) Increment k by one unit.
            '3) Increment l by one unit.
            '4) Stop the algorithm if l > L, else proceed fo the next step.
            '5) If A(i)(l) = 0 for i=k,l...,K, return to step 3. Else proceed to the next step.
            '6) Interchange the k-th equation with any equation i (with i > k) such that A(i)(l) <> 0
            '(if i=k there Is no need to perform an interchange).
            '7) For i= k + 1,... ,K, add -A(i)(l) /A(k)(l) times the k-th equation to the i-th equation.
            '8) If k < K-1, return to step 2. Else stop the algorithm

            ' 1)
            Dim k As Int32 = -1
            Dim l As Int32 = -1
            Do
                k += 1 ' 2)
                l += 1 ' 3)
                If l >= rMtx.Rows Then Exit Try ' 4)
                ' 5)
                Dim i1 As Int32 = k
                Dim j1 As Int32 = l
                ' 6) For full pivot interchanges row and/or column with greater value:
                If Not FindInversMatrix Then
                    rMtx.GetColumnPivot(i1, j1, IsAumentedMatrix, vVars, Nothing)
                Else
                    rMtx.GetColumnPivot(i1, j1, IsAumentedMatrix, vVars, invMtx)
                End If

                ' 7)
                Dim kEquation As ExprVector = ExprVector.FromExprArray(rMtx.Row(k))
                Dim kEquation2 As ExprVector = Nothing
                If FindInversMatrix Then
                    kEquation2 = ExprVector.FromExprArray(invMtx.Row(k))
                End If
                For i = k + 1 To rMtx.Rows - 1
                    Dim expr As Expression = rMtx.vExpr(i)(l)
                    If Not (expr.IsZero) Then
                        Dim iEquation As ExprVector = ExprVector.FromExprArray(rMtx.Row(i))
                        Dim iEquation2 As ExprVector = Nothing
                        If FindInversMatrix Then
                            iEquation2 = ExprVector.FromExprArray(invMtx.Row(i))
                        End If
                        Dim item_il As Expression = rMtx.Row(i)(l)
                        Dim item_kl As Expression = rMtx.Row(k)(l)
                        Dim gcd As Polynomial = Nothing
                        If item_kl.IsDouble AndAlso item_kl.IsZero Then
                            Continue For
                        End If
                        If item_il.IsPolynomial AndAlso item_kl.IsPolynomial Then
                            Dim pl As Polynomial = item_il.ToPolynomial
                            Dim pk As Polynomial = item_kl.ToPolynomial
                            gcd = Polynomial.opGcd(pl, pk)
                            If Not (gcd.IsRational AndAlso gcd.ToDouble = 1.0) Then
                                pl /= gcd : pk /= gcd
                                item_il = New Expression(pl)
                                item_kl = New Expression(pk)
                            End If
                        End If
                        Dim kEqTimes_il As ExprVector = kEquation * item_il
                        Dim iEqTimes_kl As ExprVector = iEquation * item_kl
                        iEquation = iEqTimes_kl - kEqTimes_il
                        iEquation.sortExprTerms()
                        If FindInversMatrix Then
                            Dim kEqTimes_il2 As ExprVector = kEquation2 * item_il
                            Dim iEqTimes_kl2 As ExprVector = iEquation2 * item_kl
                            iEquation2 = iEqTimes_kl2 - kEqTimes_il2
                            iEquation2.sortExprTerms()
                        End If
                        rMtx.Row(i) = iEquation.ToArray
                        If FindInversMatrix Then
                            invMtx.Row(i) = iEquation2.ToArray
                        End If
                    End If
                Next
            Loop While k < rMtx.Rows - 1
            For k = 1 To rMtx.Rows - 1
                l = ExprVector.FromExprArray(rMtx.Row(k)).NextNotZero(k)
                If l = -1 OrElse (l =
                    rMtx.Columns(k) - 1 AndAlso IsAumentedMatrix) Then Continue For
                Dim kEquation As ExprVector = ExprVector.FromExprArray(rMtx.Row(k))
                Dim kEquation2 As ExprVector = Nothing
                If FindInversMatrix Then
                    kEquation2 = ExprVector.FromExprArray(invMtx.Row(k))
                End If
                For i = k - 1 To 0 Step -1
                    Dim iEquation As ExprVector = ExprVector.FromExprArray(rMtx.Row(i))
                    Dim iEquation2 As ExprVector = Nothing
                    If FindInversMatrix Then
                        iEquation2 = ExprVector.FromExprArray(invMtx.Row(i))
                    End If
                    If Not (iEquation.Item(l).IsZero) Then
                        If Not rMtx.Row(i)(l).IsZero Then
                            Dim item_il As Expression = rMtx.Row(i)(l)
                            Dim item_kl As Expression = rMtx.Row(k)(l)
                            If item_kl.IsDouble AndAlso item_kl.IsZero Then
                                Continue For
                            End If
                            Dim gcd As Polynomial = Nothing
                            If item_il.IsPolynomial AndAlso item_kl.IsPolynomial Then
                                Dim pl As Polynomial = item_il.ToPolynomial
                                Dim pk As Polynomial = item_kl.ToPolynomial
                                gcd = Polynomial.opGcd(pl, pk)
                                If Not (gcd.IsRational AndAlso gcd.ToDouble = 1.0) Then
                                    pl /= gcd : pk /= gcd
                                    item_il = New Expression(pl)
                                    item_kl = New Expression(pk)
                                End If
                            End If
                            Dim kEqTimes_il As ExprVector = kEquation * item_il
                            Dim iEqTimes_kl As ExprVector = iEquation * item_kl
                            iEquation = iEqTimes_kl - kEqTimes_il
                            If FindInversMatrix Then
                                Dim kEqTimes_il2 As ExprVector = kEquation2 * item_il
                                Dim iEqTimes_kl2 As ExprVector = iEquation2 * item_kl
                                iEquation2 = iEqTimes_kl2 - kEqTimes_il2
                            End If
                            rMtx.Row(i) = iEquation.ToArray
                            If FindInversMatrix Then
                                invMtx.Row(i) = iEquation2.ToArray
                            End If
                        End If
                    End If
                Next
            Next
            For i = 0 To rMtx.Rows - 1
                For l = i To rMtx.Columns(i) - 1 - IIf(IsAumentedMatrix, 1, 0)
                    Dim expr_i_l As Expression = rMtx.vExpr(i)(l)
                    If Not (expr_i_l.IsZero) Then
                        Dim i_Eq As ExprVector = ExprVector.FromExprArray(rMtx.Row(i))
                        i_Eq /= expr_i_l
                        rMtx.Row(i) = i_Eq.ToArray
                        rMtx.Item(i, l) = New Expression(1.0)
                        If FindInversMatrix Then
                            Dim i_Eq2 As ExprVector = ExprVector.FromExprArray(invMtx.Row(i))
                            i_Eq2 /= expr_i_l
                            invMtx.Row(i) = i_Eq2.ToArray
                        End If
                        Exit For
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        Finally
            G10.mathml = bmathml
        End Try
        Return rMtx
    End Function
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

            'Dim oVar As New VarsAndFns(Me.cfg)
            If Me.Rows > 1 Then
                Dim sgn As Int64 = 1
                Dim maxZeros As Int64 = 0
                Dim indexZeros As Int64 = 0
                For j = 0 To Columns(0) - 1
                    Dim currZeros As Int64 = 0
                    For i = 0 To Rows - 1
                        Dim bZero As Boolean = True
                        If Item(i, j).IsPolynomial Then
                            Dim pa As Polynomial = Item(i, j).ToPolynomial
                            If Not pa.IsRational OrElse pa.ToDouble <> 0 Then
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
                        If Item(i, 0).IsPolynomial Then
                            Dim pa As Polynomial = Item(i, 0).ToPolynomial
                            If Not pa.IsRational OrElse pa.ToDouble <> 0 Then
                                bNonZero = True
                            End If
                        Else
                            bNonZero = True
                        End If
                        If Not bNonZero Then
                            For j = i + 1 To Rows - 1
                                bNonZero = False
                                If Item(j, 0).IsPolynomial Then
                                    Dim pa As Polynomial = Item(j, 0).ToPolynomial
                                    If Not pa.IsRational OrElse pa.ToDouble <> 0 Then
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
                retDetermExpr = New Expression(Item(0, 0))
                Do
                    Dim bContinue As Boolean = False
                    For i = Rows - 1 To 0 Step -1
                        Dim bNonZero As Boolean = False
                        If Item(i, 0).IsPolynomial Then
                            Dim pa As Polynomial = Item(i, 0).ToPolynomial
                            If Not (pa.IsRational AndAlso pa.ToDouble = 0.0) Then
                                bNonZero = True
                            End If
                        Else
                            bNonZero = True
                        End If
                        If bNonZero AndAlso i Then
                            Dim v As New ExprMatrix(Row(i - 1))
                            Dim v2 As New ExprMatrix(Row(i))
                            Dim ai As Expression = Item(i, 0)
                            Dim k As Int32 = i - 1
                            Dim aimn1 As Expression = Item(k, 0)
                            Do While k > 0 AndAlso aimn1.IsDouble AndAlso aimn1.ToDouble = 0.0
                                k -= 1
                                aimn1 = Item(k, 0)
                            Loop
                            If Not (aimn1.IsDouble AndAlso aimn1.ToDouble = 0.0) Then
                                If ai.IsPolynomial AndAlso aimn1.IsPolynomial Then
                                    Dim gcd As Polynomial = Polynomial.opGcd(ai.ToPolynomial, aimn1.ToPolynomial)
                                    If gcd IsNot Nothing AndAlso Not (gcd.IsRational AndAlso gcd.ToDouble = 1.0) Then
                                        ai /= New Expression(gcd)
                                        aimn1 /= New Expression(gcd)
                                    End If
                                End If
                                retDetermExpr /= aimn1
                                v *= ai
                                v2 *= aimn1
                                v2 -= v
                                For j = 0 To Columns(0) - 1
                                    Item(i, j) = ReduceExprUsingPolynomials.TryToReduce(v2.Item(0, j))
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
                            curr *= Item(i, permI(i))
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
            Throw
        End Try
        Try
            If bGetInverseOrCofactorMtx Then
                Dim Adj(N - 1, N - 1) As ExprMatrix
                If N = 1 Then
                    Adj(0, 0) = New ExprMatrix(Me.Item(0, 0))
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
                                                Adj(iRow, iCol) = New ExprMatrix(N - 1, N - 1)
                                            End If
                                            If (i + j) Mod 2 Then
                                                Adj(iRow, iCol).Item(i1, j1) =
                                                    New Expression(-cpyeMtx.Item(i, j))
                                            Else
                                                Adj(iRow, iCol).Item(i1, j1) =
                                                    New Expression(cpyeMtx.Item(i, j))
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
                InvMtx = New ExprMatrix(N, N)
                For i = 0 To N - 1
                    For j = 0 To N - 1
                        InvMtx.Item(i, j) = Adj(i, j).opDeterminant
                    Next
                Next
                cofactorMtx = InvMtx
                adjoint = ExprMatrix.opTranspose(InvMtx)
                ' Finally, transpose InvMtx and divide each
                ' entry by the determinant (retExpr):
                InvMtx = ExprMatrix.opTranspose(InvMtx, retDetermExpr)
            End If

        Catch ex As Exception
            Throw
        End Try

        Return retDetermExpr
    End Function
    Public Shared Function opTranspose(ByVal eMa As ExprMatrix,
               Optional divideBy As Expression = Nothing) As ExprMatrix
        Dim eMc As New ExprMatrix(eMa.Columns(0), eMa.Rows)
        Try
            Dim i As Int64
            For iCol As Int64 = 0 To eMa.Columns(0) - 1
                For i = 0 To eMa.Rows - 1
                    If eMa.Item(i, iCol) IsNot Nothing Then
                        If divideBy Is Nothing Then
                            eMc.Item(iCol, i) =
                            New Expression(eMa.Item(i, iCol))
                        Else
                            eMc.Item(iCol, i) =
                            eMa.Item(i, iCol) / divideBy
                        End If
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Function

    Public Function opMenor(iRow As Int64, iCol As Int64) As ExprMatrix
        Dim i, j As Int64
        Dim i1 As Int64 = 0
        Dim Adj As New ExprMatrix(Rows - 1, Columns(0) - 1)
        For i = 0 To Rows - 1
            If i <> iRow Then ' exclude row = iRow
                Dim j1 As Int64 = 0
                For j = 0 To Columns(0) - 1
                    If j <> iCol Then ' exclude col = iCol
                        Adj.Item(i1, j1) = New Expression(Me.Item(i, j))
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
            For j = 0 To Columns(0) - 1
                Dim aux As Expression = Item(interchageI, j)
                Item(interchageI, j) = Item(withJ, j)
                Item(withJ, j) = aux
            Next
        Else
            For i = 0 To Rows - 1
                Dim aux As Expression = Item(i, interchageI)
                Item(i, interchageI) = Item(i, withJ)
                Item(i, withJ) = aux
            Next
        End If
    End Sub
    Public Shared Function opCrossProduct(ByVal eMa As ExprMatrix,
               Optional divideBy As Expression = Nothing) As ExprMatrix
        Dim eMc As New ExprMatrix(1, 3) ' 1 row, 3 columns
        Try
            ' | i   j   k  |
            '
            ' |a00 a01 a02 |
            ' |a10 a11 a12 |
            Dim a00 As New Expression(eMa.Item(0, 0))
            Dim a01 As New Expression(eMa.Item(0, 1))
            Dim a02 As New Expression(eMa.Item(0, 2))
            Dim a10 As New Expression(eMa.Item(1, 0))
            Dim a11 As New Expression(eMa.Item(1, 1))
            Dim a12 As New Expression(eMa.Item(1, 2))
            eMc.Item(0, 0) = a01 * a12 - a02 * a11
            eMc.Item(0, 1) = -(a00 * a12 - a02 * a10)
            eMc.Item(0, 2) = a00 * a11 - a01 * a10
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Function
    Public Shared Function opDotProduct(ByVal eMa As ExprMatrix,
               Optional divideBy As Expression = Nothing) As ExprMatrix
        Dim eMc As New ExprMatrix(1, 1) ' 1 row, 1 column
        Try
            'Dim i As Int64
            '
            ' |a00 a01 a02 |
            ' |a10 a11 a12 |
            Dim a00 As New Expression(eMa.Item(0, 0))
            Dim a01 As New Expression(eMa.Item(0, 1))
            Dim a02 As New Expression(eMa.Item(0, 2))
            Dim a10 As New Expression(eMa.Item(1, 0))
            Dim a11 As New Expression(eMa.Item(1, 1))
            Dim a12 As New Expression(eMa.Item(1, 2))
            eMc.Item(0, 0) = a00 * a10 + a01 * a11 + a02 * a12
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Function
    Public Function OpMax() As Double
        Dim max As Double = Double.MinValue
        Try
            For i As Int32 = 0 To vExpr.Length - 1
                For j As Int32 = 0 To vExpr(i).Length - 1
                    If vExpr(i)(j).IsDouble Then
                        Dim cur As Double = vExpr(i)(j).ToDouble
                        If cur > max Then
                            max = cur
                        End If
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return max
    End Function
    Public Function OpMin() As Double
        Dim min As Double = Double.MaxValue
        Try
            For i As Int32 = 0 To vExpr.Length - 1
                For j As Int32 = 0 To vExpr(i).Length - 1
                    If vExpr(i)(j).IsDouble Then
                        Dim cur As Double = vExpr(i)(j).ToDouble
                        If cur < min Then
                            min = cur
                        End If
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return min
    End Function
    Public Shared Function Identity(rows As Int64, Optional cols As Int64 = 0) As ExprMatrix
        ' see http://en.wikipedia.org/wiki/Identity_matrix
        Dim emIdentity As ExprMatrix = Nothing
        Try
            If cols = 0 Then cols = rows
            emIdentity = New ExprMatrix(rows, cols)
            For j As Int64 = 0 To rows - 1
                For k As Int64 = 0 To cols - 1
                    If j = k Then
                        emIdentity.Item(j, k) = New Expression(1.0)
                    Else
                        emIdentity.Item(j, k) = New Expression(0.0)
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return emIdentity
    End Function
    Public Shared Function Zero(rows As Int64, Optional cols As Int64 = 0) As ExprMatrix
        Dim emIdentity As ExprMatrix = Nothing
        Try
            If cols = 0 Then cols = rows
            emIdentity = New ExprMatrix(rows, cols)
            For j As Int64 = 0 To rows - 1
                For k As Int64 = 0 To cols - 1
                    emIdentity.Item(j, k) = New Expression(0.0)
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return emIdentity
    End Function

    Public Function opRank() As Int64
        Dim r As Int64 = 0
        Try
            Dim Echelon As New ExprMatrix(Me)
            Echelon = ExprMatrix.GaussianElimination(Echelon, False, Nothing, False, Nothing) ' opEchelonForm()
            Dim i, j As Int64
            For i = Echelon.Rows - 1 To 0 Step -1
                For j = 0 To Echelon.Columns(0) - 1
                    If Not Echelon.Item(i, j).IsDouble OrElse
                    Echelon.Item(i, j).ToDouble <> 0 Then
                        r = i + 1
                        Exit Try
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return r
    End Function
    Public Function opEigenValues(bReturnAsDiagonalMatrix As Boolean) As ExprMatrix
        Dim eMtx As ExprMatrix = Nothing
        Try
            ' Finding the eigenvalues of matrix A, is
            ' equivalent to find the roots of det(A-lambda*I), where I
            ' is the identity matrix. (Do a search for eigenvalues).
            If Me.Rows <> Me.Columns(0) Then
                Throw New Exception(Msg8.msg(41, dspNumError:=False)) ' matrix is not squared
            End If
            Dim lamda As New Expression(New Polynomial("_λ", 1))
            ' Presuming the determinant will be a polynomial:
            Dim eMtx1 As ExprMatrix = (Me - Identity(Me.Rows) * lamda)
            Dim eDet As Expression = eMtx1.opDeterminant
            Dim Pa As Polynomial = eDet.ToPolynomial
            Pa.Roots()
            Dim roots() As Complex = Pa.pRootsNum ' Polynomial.opRoots(Pa).cjo
            Dim rows As Int64 = roots.Length
            Dim cols As Int64 = rows
            If Not bReturnAsDiagonalMatrix Then
                cols = 1
                eMtx = New ExprMatrix(rows, cols)
                For i As Int64 = 0 To rows - 1
                    eMtx.Item(i, 0) = New Expression(roots(i))
                Next
            Else
                eMtx = New ExprMatrix(rows, cols)
                For i As Int64 = 0 To rows - 1
                    For j As Int64 = 0 To cols - 1
                        If i = j Then
                            eMtx.Item(i, j) = New Expression(roots(i))
                        Else
                            eMtx.Item(i, j) = New Expression(0.0)
                        End If
                    Next
                Next
            End If
        Catch ex As Exception
            Throw
        End Try
        Return eMtx
    End Function
    Public Shared Sub resolveDeterminateSysOfEqs(
                            ByRef sysOfEqs As ExprMatrix,
                            ByRef vVar() As String,
                            ByRef vValues() As Expression,
                            Optional b As ExprMatrix = Nothing)

        Try
            ' sysOfEqs is the matrix with the coefficients of the variables
            ' b is the matrix of independent values (coefficients or constants)
            ' For the system:
            '     x + 2y + z = 3
            '     x - 2y -2z = 8
            '    2x + 4y +2z = 6
            ' sysOfEqs = (1,2,1|1,-2,-2|2,4,2) and b=(3|8|6)

            Dim lstVars As New List(Of String)
            ReDim vValues(-1)
            Dim j As Int64
            Dim rows As Int64 = sysOfEqs.Rows
            Dim cols As Int64 = sysOfEqs.Columns(0)
            Dim vIndexRows(rows - 1), iv As Int64
            Dim usedRows(rows - 1) As Int64
            Dim bIsComplex As Boolean = True
            For j = 0 To cols - 1
                Dim indexRow As Int64 = -1
                ' Look for pivot:
                Dim max As Double = 0
                For row As Int64 = 0 To rows - 1
                    If usedRows(row) = 0 Then
                        Dim cExpr1 As Expression = sysOfEqs.Item(row, j)
                        If Not cExpr1.IsComplex Then
                            indexRow = row
                            Exit For
                        Else
                            If cExpr1.ToComplex.opModulo > max Then
                                max = cExpr1.ToComplex.opModulo
                                indexRow = row
                            End If
                        End If
                    End If
                Next
                If indexRow = -1 Then
                    GoTo sigJ
                End If
                ' Mark current row:
                usedRows(indexRow) = 1
                vIndexRows(iv) = indexRow
                iv += 1
                ' Zero rows not marked and with coeff. j not zero:
                Dim mxExpr As Expression = sysOfEqs.Item(indexRow, j)
                For row As Int64 = 0 To rows - 1
                    If usedRows(row) = 0 Then ' not marked?
                        Dim cExpr As Expression = sysOfEqs.Item(row, j)
                        If Not cExpr.IsComplex OrElse cExpr.ToComplex.opModulo <> 0.0 Then ' not zero?
                            ' Zero column j of row #row:
                            Dim vect1(cols - 1) As Expression
                            Dim vect2(cols - 1) As Expression
                            'Dim cExpr2 As Expression = sysOfEqs.Item(row, j)
                            For col As Int64 = 0 To cols - 1
                                vect1(col) = mxExpr * sysOfEqs.Item(row, col)
                                vect2(col) = cExpr * sysOfEqs.Item(indexRow, col)
                                sysOfEqs.Item(row, col) = vect1(col) - vect2(col)
                                If b IsNot Nothing Then
                                    ' operate similarly over b:
                                    vect1(col) = mxExpr * b.Item(row, 0)
                                    vect2(col) = cExpr * b.Item(indexRow, 0)
                                    b.Item(row, 0) = vect1(col) - vect2(col)
                                End If
                            Next
                        End If
                    End If
                Next
sigJ:
            Next
            For row As Int64 = 0 To rows - 1
                lstVars.Add("x" + row.ToString)
            Next
            Dim nZeros(cols - 1) As Int64
            Dim vMtxR(rows - 1) As Object
            For row As Int64 = 0 To iv - 1
                Dim MtxR(cols - 1) As Expression
                Dim i As Int64 = vIndexRows(row)
                Dim cExpr As New Expression(0.0)
                For col As Int64 = 0 To cols - 1
                    Dim colExpr As Expression = sysOfEqs.Item(i, col)
                    If Not (colExpr.IsDouble AndAlso colExpr.ToDouble = 0) Then
                        MtxR(col) = New Expression(sysOfEqs.Item(i, col))
                        If Not colExpr.IsDouble OrElse
                        colExpr.ToDouble <> 0.0 Then
                            sysOfEqs.Item(i, col) /= colExpr
                            MtxR(col) = New Expression(sysOfEqs.Item(i, col))
                            For col1 As Int64 = col + 1 To cols - 1
                                sysOfEqs.Item(i, col1) /= colExpr
                                MtxR(col1) = New Expression(sysOfEqs.Item(i, col1))
                            Next
                            Exit For
                        End If
                    End If
                    nZeros(i) += 1
                Next
                vMtxR(i) = MtxR
                If b IsNot Nothing Then
                    cExpr = b.Item(i, 0)
                End If
                For col As Int64 = nZeros(i) + 1 To cols - 1
                    cExpr -= sysOfEqs.Item(i, col) *
                        New Expression(New Polynomial("x" + col.ToString, 1))
                Next

                Dim Id As Int64 = -1
                Id = Array.IndexOf(lstVars.ToArray, "x" + nZeros(i).ToString)
                If Id > -1 Then
                    If Id >= vValues.Length Then
                        ReDim Preserve vValues(Id)
                    End If
                    vValues(Id) = cExpr
                Else
                    Exit For
                End If
            Next
            ReDim Preserve nZeros(rows - 1)
            For row As Int64 = 0 To vMtxR.Length - 1
                If vMtxR(row) Is Nothing Then
                    Dim mtxR(cols - 1) As Expression
                    For col As Int64 = 0 To cols - 1
                        mtxR(col) = New Expression(0.0)
                    Next
                    vMtxR(row) = mtxR
                    nZeros(row) = cols
                End If
            Next
            Array.Sort(nZeros, vMtxR)
            For row As Int64 = 0 To rows - 1
                For col As Int64 = 0 To cols - 1
                    If vMtxR(row)(col) IsNot Nothing Then
                        sysOfEqs.Item(row, col) = CType(
                           vMtxR(row)(col), Expression)
                    Else
                        sysOfEqs.Item(row, col) =
                            New Expression(0.0)
                    End If
                Next
            Next
            vVar = lstVars.ToArray
        Catch ex As Exception
            Throw
        End Try
    End Sub
    Public Sub setExprTo(ByVal rows As Int64, ByVal cols As Int64)
        ReDim Preserve vExpr(rows - 1)
        For i As Int32 = 0 To rows - 1
            ReDim Preserve vExpr(i)(cols - 1)
        Next
    End Sub

    Public Function opEigenVectors(Optional bNormalize As Boolean = False) As ExprMatrix
        Dim eMtx As ExprMatrix = Nothing
        Dim bVar As Boolean = G10.var
        G10.var = True
        Try
            G10.Initialize()
            Dim vVars() As String = Nothing '= getAllVars
            Me.IsPolynomialMatrix(vVars) ' all vars retreived in vVars
            eMtx = New ExprMatrix(Rows, Columns(0))
            ' 1) Obtain the eigenvalues
            Dim eigenValues As ExprMatrix = opEigenValues(False)
            '   Roots are sorted, which implies that
            '   equal roots are consecutive in opEigenValues()
            Dim vRoot(0) As Complex, vMultiplicity(0) As Int64
            Dim i, j As Int64, iR As Int64 = 0
            vRoot(0) = New Complex(eigenValues.Item(0, 0).ToComplex)
            vMultiplicity(0) = 1
            For i = 1 To eigenValues.Rows - 1
                If (vRoot(iR) - eigenValues.Item(i, 0).ToComplex).IsZero Then
                    ' root is repeated and so, increment its multiplicity:
                    vMultiplicity(iR) += 1
                Else
                    ' a different root --> add to vRoot()
                    iR += 1
                    ReDim Preserve vRoot(iR), vMultiplicity(iR)
                    vMultiplicity(iR) = 1
                    vRoot(iR) = New Complex(eigenValues.Item(i, 0).ToComplex)
                End If
            Next
            ' 2) Solve A*v = lambda * v for each lambda in vRoot()
            Dim curVector As Int64 = 0
            For i = 0 To iR
                ' (A-lambda*I)*v = 0
                Dim lambda As New Expression(vRoot(i))
                Dim eqMtx As ExprMatrix = Me - Identity(Rows) * lambda
                Dim rank As Int64 = eqMtx.opRank
                Dim nullDim As Int64 = Me.Rows - rank
                If nullDim = 0 Then nullDim = 1
                Dim b As ExprMatrix = ExprMatrix.Zero(Rows, 1)
                For row As Int64 = 0 To Rows - 1
                    b.Item(row, 0) = New Expression(0.0)
                Next
                Dim eqMtx1 As New ExprMatrix(eqMtx)
                Dim vValues() As Expression = Nothing
                resolveDeterminateSysOfEqs(eqMtx1, vVars, vValues, b)
                Dim eigenVector As New ExprMatrix(Rows, 1)
                Dim iM As Int64 = 0
                For j = 0 To Columns(0) - 1
                    Dim nom As String = "x" + j.ToString
                    Dim id As Int64 = Array.IndexOf(vVars, nom) ' oVar.getVarIDByName(nom)
                    If id >= vValues.Length Then
                        ReDim Preserve vValues(id)
                    End If
                    If vValues(id) Is Nothing Then
                        vValues(id) = New Expression(1.0)
                    End If
                Next
                Dim pE As New ParseExpression
                Dim lstVars As New Dictionary(Of String, Expression)
                For j = 0 To Columns(0) - 1
                    lstVars.Add(vVars(j), vValues(j))
                Next
                For j = 0 To Columns(0) - 1
                    vValues(j) = pE.Evaluate(lstVars, "x" + j.ToString).vExpr(0)
                Next
                For j = 0 To Columns(0) - 1
                    Dim nom As String = "x" + j.ToString
                    Dim id As Int64 = Array.IndexOf(vVars, nom) ' = oVar.getVarIDByName(nom)
                    If Not vValues(id).IsComplex Then
                        vValues(id) = New Expression(1.0)
                    End If
                    lstVars(nom) = vValues(id)
                Next
                For j = 0 To Columns(0) - 1
                    vValues(j) = pE.Evaluate(lstVars, "x" + j.ToString).vExpr(0)
                Next
                For iNull As Int64 = 1 To nullDim
                    For j = 0 To Columns(0) - 1
                        Dim nom As String = "x" + j.ToString
                        Dim id As Int64 = Array.IndexOf(vVars, nom) ' = oVar.getVarIDByName(nom)
                        Dim cExpr As Expression = vValues(id) ' = oVar.getValueByID(id).Item(0, 0)
                        eMtx.Item(j, curVector) = New Expression(cExpr)
                        eigenVector.Item(j, 0) = eMtx.Item(j, curVector)
                    Next
                    curVector += 1
                    iM += 1
                Next

                ' generalized eigenvectors, if any:
                For iM2 As Int64 = iM To vMultiplicity(i) - 1
                    ReDim vVars(-1), vValues(-1) ' oVar = Nothing
                    resolveDeterminateSysOfEqs(eqMtx, vVars, vValues, eigenVector)
                    For j = 0 To Columns(0) - 1
                        Dim nom As String = "x" + j.ToString
                        Dim id As Int64 = Array.IndexOf(vVars, nom) '= oVar.getVarIDByName(nom)
                        If vValues(id) Is Nothing Then
                            vValues(id) = New Expression(1.0)
                        End If
                    Next
                    For j = 0 To Columns(0) - 1
                        Dim cExpr As Expression = vValues(j) ' = oVar.getValueByID(j).Item(0, 0)
                        eMtx.Item(j, curVector) = New Expression(cExpr)
                        eigenVector.Item(j, 0) = eMtx.Item(j, curVector)
                    Next
                Next
            Next
            For j = 0 To eMtx.Columns(0) - 1
                If eMtx.Item(0, j) Is Nothing Then
                    eMtx.setExprTo(eMtx.Rows, j)
                    Exit For
                End If
            Next

            ' normalize
            If bNormalize Then
                For j = 0 To eMtx.Columns(0) - 1
                    Dim norm As Double = 0.0
                    For i = 0 To eMtx.Rows - 1
                        norm += eMtx.Item(i, j).ToDouble ^ 2
                    Next
                    norm = norm ^ 0.5
                    Dim normExpr As New Expression(norm)
                    For i = 0 To eMtx.Rows - 1
                        eMtx.Item(i, j) /= normExpr
                    Next
                Next
            End If
        Catch ex As Exception
            Throw
        Finally
            G10.var = bVar
        End Try
        Return eMtx
    End Function
    Public Function opTrace() As ExprMatrix
        ' see http://en.wikipedia.org/wiki/Trace_(linear_algebra)
        Dim r As ExprMatrix = Nothing
        Try
            Dim expr As New Expression(0.0)
            For i = 0 To Rows - 1
                expr += Item(i, i)
            Next
            r = New ExprMatrix(expr)
        Catch ex As Exception
            Throw
        End Try
        Return r
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
            vRoot(0) = New Complex(eigenValues.Item(0, 0).ToComplex)
            vMultiplicity(0) = 1
            For i = 1 To eigenValues.Rows - 1
                If (vRoot(iR) - eigenValues.Item(i, 0).ToComplex).IsZero Then
                    ' root is repeated and so, increment its multiplicity:
                    vMultiplicity(iR) += 1
                Else
                    ' a different root --> add to vRoot()
                    iR += 1
                    ReDim Preserve vRoot(iR), vMultiplicity(iR)
                    vMultiplicity(iR) = 1
                    vRoot(iR) = New Complex(eigenValues.Item(i, 0).ToComplex)
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
            Jordan = Identity(n)
            Dim curEntry As Int64 = 0
            For k As Int64 = 0 To iR   ' for each lambda_k...
                ReDim Preserve vN(k)(0)
                ' compute vN(k)(0) = dim(N(A-lambda_k)) = n -rank(...
                Dim AminusLambda As New ExprMatrix(
                        Me - Identity(n) * New Expression(vRoot(k)))
                vN(k)(0) = n - AminusLambda.opRank
                If vN(k)(0) = vMultiplicity(k) Then
                    ' One diagonal block of vMulitiplicity(k) size and
                    ' vRoot(k) at each diagonal entry:
                    For i = 0 To vMultiplicity(k) - 1
                        Jordan.Item(curEntry, curEntry) = New Expression(vRoot(k))
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
                            Jordan.Item(curEntry, curEntry) = New Expression(vRoot(k))
                            curEntry += 1
                            If curEntry >= Rows Then GoTo sigI
                        Next
                    End If
                    For j As Int64 = 0 To ivN
                        For iBlock As Int64 = 1 To nBlocks(j) ' nBlocks(j) of size = vBlockSize(j)
                            For i = 0 To vBlockSize(j) - 1 ' a block of size = vBlockSize(j)
                                Jordan.Item(curEntry, curEntry) = New Expression(vRoot(k))
                                If curEntry + 1 >= Columns(0) Then GoTo sigI
                                If i < vBlockSize(j) - 1 AndAlso
                                curEntry + 1 < Columns(0) Then
                                    ' if not the last row, set value '1' to
                                    ' the entry at the right of the diagonal (i.e.
                                    ' at the superdiagonal):
                                    Jordan.Item(curEntry, curEntry + 1) =
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
            Throw
        End Try
        Return Jordan
    End Function
    Public Shared Function resolveLinearSystemOfEquations(eM As ExprMatrix, Optional eMvars() As String = Nothing) As ExprMatrix
        Dim ret As ExprMatrix
        Try
            Dim eMtx As ExprMatrix
            Dim vVars() As String = eMvars
            If vVars Is Nothing Then
                vVars = eM.GetVars
            End If
            Array.Sort(vVars)
            Dim vVars2(vVars.Length - 1)
            Array.Copy(vVars, vVars2, vVars.Length)
            Dim augmentedMtx As ExprMatrix = Nothing
            eM.GetAugmentedMatrixOutFromVars(vVars, augmentedMtx)
            eMtx = ExprMatrix.GaussianElimination(augmentedMtx, True, vVars, False, Nothing)
            Dim i0, j, k, k2 As Int32
            For i = 0 To vVars.Length - 1
                If eMtx.Item(i, i).IsZero Then ' variable vVars(i) is independent
                    For j = i To vVars.Length - 2
                        For i0 = eMtx.Rows - 1 To i + 1 Step -1
                            For k = 0 To vVars.Length
                                eMtx.Item(i0, k) = eMtx.Item(i0 - 1, k)
                            Next
                        Next
                        If Not eMtx.Item(i, j).IsZero Then Exit For
                    Next
                    For j = i + 1 To vVars.Length - 1
                        eMtx.Item(i, j) = New Expression(0.0)
                    Next
                    ' Assign an arbitrary value _λx
                    eMtx.Item(i, i) = New Expression(1.0)
                    Dim nom As String = "λ" + (1 + k2).ToString
                    Do While Array.IndexOf(vVars, nom) > -1
                        k2 += 1
                        nom = "λ" + (1 + k2).ToString
                    Loop
                    eMtx.Item(i, vVars.Length) = New Expression(nom, 1)
                    vVars2(i) = nom
                End If
            Next
            Dim expr As New Expression(0.0)
            For i0 = 0 To vVars.Length - 1
                For j = i0 + 1 To eMtx.Columns(i0) - 2
                    Dim cjo As Complex = eMtx.Item(i0, j).ToComplex
                    If Not cjo.IsZero Then
                        eMtx.Item(i0, vVars.Length) -=
                                        New Expression(vVars2(j), 1) * New Expression(cjo)
                    End If
                Next
            Next
            ret = New ExprMatrix(vVars.Length, 2)
            Dim vars As New Dictionary(Of String, Expression)
            Dim pE As New ParseExpression
            For i As Int32 = 0 To vVars.Length - 1
                ret.Item(i, 0) = New Expression(vVars(i) + "=", 1)
                Dim s As String = eMtx.Item(i, vVars.Length).ToString()
                Dim exprS As Expression = pE.Evaluate(vars, s.Replace(" ", "")).vExpr(0)
                vars.Add(vVars(i), exprS)
            Next
            Dim b As Boolean = G10.mathml
            G10.mathml = False
            For i = vVars.Length - 1 To 0 Step -1
                Dim s As String = eMtx.Item(i, vVars.Length).ToString
                s = s.Replace(" ", "")
                expr = pE.Evaluate(vars, s).vExpr(0)
                ret.Item(i, 1) = expr
            Next
            G10.mathml = b
        Catch ex As Exception
            Throw
        End Try
        Return ret
    End Function
    Public Function opJacobian(ByVal vVars() As String) As ExprMatrix
        Dim eMc As ExprMatrix = Nothing
        Try
            eMc = New ExprMatrix(Me.Rows, Columns(0))
            Dim col As Int64
            For row As Int64 = 0 To Me.Rows - 1
                For col = 0 To Columns(0) - 1
                    Dim expr As Expression = Item(row, col)
                    If expr IsNot Nothing Then
                        eMc.Item(row, col) = expr.Derivative(vVars(col))
                    End If
                Next
            Next
        Catch ex As Exception
            Throw
        End Try
        Return eMc
    End Function
    Public Shared Function LCM_GCD(a() As Double, Optional ByRef sDetail As String = "") As Double()
        Dim dbRet(1) As Double
        Try
            ' Finds Least Common Multiple (LCM) and
            ' Greatest Common Divisor (GCD) of a series of 
            ' numbers present in a()

            ' Return values:
            ' element dbRet(0)<--GCD , dbRet(1)<--- LCM
            Dim cols As Int64 = a.Length
            Dim vExp(cols - 1)() As Int64, iv As Int64 = 0
            For i As Int64 = 0 To cols - 1
                Dim curDb As Double = a(i)
                If curDb < 1.0 Then
                    Throw New Exception(Msg8.num(9)) ' Argument(s) not valid.
                End If
                For j As Int64 = 0 To Rational.vPrime.Length - 1
                    Dim prime As Double = Rational.vPrime(j)
                    Do While Math.Floor(curDb / prime) = curDb / prime
                        curDb /= prime
                        If j > iv Then
                            iv = j
                        End If
                        ReDim Preserve vExp(i)(iv)
                        vExp(i)(j) += 1
                        If curDb = 1.0 Then Exit For
                    Loop
                Next
            Next
            Dim vMax(iv) As Int64
            Dim vMin(iv) As Int64
            For j As Int64 = 0 To iv
                vMin(j) = Int64.MaxValue
                For i As Int64 = 0 To cols - 1
                    ReDim Preserve vExp(i)(iv)
                    If vExp(i)(j) > vMax(j) Then
                        vMax(j) = vExp(i)(j)
                    End If
                    If vExp(i)(j) < vMin(j) Then
                        vMin(j) = vExp(i)(j)
                    End If
                Next
            Next
            Dim lcm As Double = 1
            Dim gcd As Double = 1
            For j = 0 To iv
                If vMax(j) Then
                    lcm *= Rational.vPrime(j) ^ vMax(j)
                End If
                If vMin(j) Then
                    gcd *= Rational.vPrime(j) ^ vMin(j)
                End If
            Next
            dbRet(0) = gcd
            dbRet(1) = lcm
        Catch ex As Exception
            Throw
        End Try
        Return dbRet
    End Function

End Class
