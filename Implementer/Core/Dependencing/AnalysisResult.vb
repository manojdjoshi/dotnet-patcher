Namespace Core.Dependencing

    Public NotInheritable Class AnalysisResult

#Region " Properties "
        Public ReadOnly Property result As String
#End Region

#Region " Constructor "
        Friend Sub New(result As String)
            _result = result
        End Sub
#End Region

    End Class

End Namespace
