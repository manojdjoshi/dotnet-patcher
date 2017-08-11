Namespace Core.Versions
    Public Class Infos
        Implements IDisposable

#Region " Properties "

        Public ReadOnly Property Enabled As Boolean
        Public ReadOnly Property FileDescription As String
        Public ReadOnly Property Comments As String
        Public ReadOnly Property CompanyName As String
        Public ReadOnly Property ProductName As String
        Public ReadOnly Property LegalCopyright As String
        Public ReadOnly Property LegalTrademarks As String
        Public ReadOnly Property FileVersion As String
        Public ReadOnly Property ProductVersion As String
#End Region

#Region " Constructor "
        Public Sub New(Enabl As Boolean, FileDescript$, Comment$, CompanyN$, ProductN$, LegalCopy$, LegalTrade$, FileV$, ProductV$)
            _Enabled = Enabl
            _FileDescription = FileDescript
            _Comments = Comment
            _CompanyName = CompanyN
            _ProductName = ProductN
            _LegalCopyright = LegalCopy
            _LegalTrademarks = LegalTrade
            _FileVersion = FileV
            _ProductVersion = ProductV
        End Sub

        Public Sub New(Enabl As Boolean, FilePath$)
            Dim fvi As FileVersionInfo = FileVersionInfo.GetVersionInfo(FilePath)
            _Enabled = Enabl
            _FileDescription = fvi.FileDescription
            _Comments = fvi.Comments
            _CompanyName = fvi.CompanyName
            _ProductName = fvi.ProductName
            _LegalCopyright = fvi.LegalCopyright
            _LegalTrademarks = fvi.LegalTrademarks
            _FileVersion = fvi.FileVersion
            _ProductVersion = fvi.ProductVersion
        End Sub
#End Region

#Region " Methods "

        Private Sub CleanUp()
            _Enabled = False
            _FileDescription = String.Empty
            _Comments = String.Empty
            _CompanyName = String.Empty
            _ProductName = String.Empty
            _LegalCopyright = String.Empty
            _LegalTrademarks = String.Empty
            _FileVersion = String.Empty
            _ProductVersion = String.Empty
        End Sub

        Private Function checkVersion(VersionValue$) As String
            If (VersionValue <> String.Empty) Then
                If Not VersionValue.Contains(".") Then Return "0.0.0.0"
                If (VersionValue.Split(New Char() {"."c}).Length = 4) Then Return VersionValue
            End If
            Return "0.0.0.0"
        End Function

#End Region

#Region "IDisposable Support"
        Private disposedValue As Boolean

        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                End If
                CleanUp()
            End If
            Me.disposedValue = True
        End Sub

        Public Sub Dispose() Implements IDisposable.Dispose
            Dispose(True)
            GC.SuppressFinalize(Me)
        End Sub
#End Region

    End Class
End Namespace
