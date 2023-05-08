
<Serializable()> _
Public Class C_session
    Public Shared xi1(), xf1(), yAuto1() As Double
    Public Shared psfn(), psf2(), psf3() As String
    Public Shared Property sfn(id As Int32) As String
        Get
            Return psfn(id)
        End Get
        Set(value As String)
            psfn(id) = value
        End Set
    End Property
    Public Shared Property sf2(id As Int32) As String
        Get
            Return psf2(id)
        End Get
        Set(value As String)
            psf2(id) = value
        End Set
    End Property
    Public Shared Property sf3(id As Int32) As String
        Get
            Return psf3(id)
        End Get
        Set(value As String)
            psf3(id) = value
        End Set
    End Property
    Public Shared Property xi(ByVal id As Int32) As Double
        Get
            Return xi1(id)
        End Get
        Set(ByVal value As Double)
            xi1(id) = value
        End Set
    End Property
    Public Shared Property xf(ByVal id As Int32) As Double
        Get
            Return xf1(id)
        End Get
        Set(ByVal value As Double)
            xf1(id) = value
        End Set
    End Property
    Public Shared Property yAuto(ByVal id As Int32) As Double
        Get
            Return yAuto1(id)
        End Get
        Set(ByVal value As Double)
            yAuto1(id) = value
        End Set
    End Property
End Class
