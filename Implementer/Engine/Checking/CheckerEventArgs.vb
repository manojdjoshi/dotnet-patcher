Namespace Engine.Checking

#Region " Delegates "
    Public Delegate Sub Check(sender As Object, e As CheckEventArgs)
#End Region

    Public NotInheritable Class CheckEventArgs
        Inherits EventArgs

#Region " Properties "
        Public ReadOnly Property message As String
        Public ReadOnly Property title As String
        Public ReadOnly Property checkedFile As String
#End Region

#Region " Constructor "
        Public Sub New(message As String, title As String, checkedFile As String)
            _message = message
            _title = title
            _checkedFile = checkedFile
        End Sub
#End Region

    End Class
End Namespace
