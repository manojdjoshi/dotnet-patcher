Namespace Core.Packer
    Public NotInheritable Class ZipInfos

#Region " Properties "
        Friend ReadOnly Property fPath As String
        Friend ReadOnly Property refByte As Byte()
        Friend ReadOnly Property refNewNamespaceName As String
        Friend ReadOnly Property refNewTypeName As String
        Friend ReadOnly Property refNewMethodName As String
#End Region

#Region " Constructor "

        Friend Sub New(filePath$, rByte As Byte(), refNewNamespaceName$, refNewTypeName$, refNewMethodName$)
            _fPath = filePath
            _refByte = rByte
            _refNewNamespaceName = refNewNamespaceName
            _refNewTypeName = refNewTypeName
            _refNewMethodName = refNewMethodName
        End Sub

#End Region

    End Class
End Namespace
