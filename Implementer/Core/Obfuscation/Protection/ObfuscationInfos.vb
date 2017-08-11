Namespace Core.Obfuscation.Protection
    Public Class ObfuscationInfos
        Implements IDisposable

#Region " Properties "
        Public ReadOnly Property Enabled As Boolean
        Public ReadOnly Property RenameResourcesContent As Boolean
        Public ReadOnly Property EncryptResources As Boolean
        Public ReadOnly Property CompressResources As Boolean
        Public ReadOnly Property AntiIlDasm As Boolean
        Public ReadOnly Property AntiTamper As Boolean
        Public ReadOnly Property AntiDebug As Boolean
        Public ReadOnly Property AntiDumper As Boolean
        Public ReadOnly Property EncryptBoolean As Boolean
        Public ReadOnly Property EncryptNumeric As Boolean
        Public ReadOnly Property EncryptString As Boolean
        Public ReadOnly Property HidePublicCalls As Boolean
        Public ReadOnly Property ControlFlow As Boolean
        Public ReadOnly Property InvalidMetadata As Boolean
        Public ReadOnly Property RenameAssembly As Boolean
#End Region

#Region " Constructor "
        Public Sub New(Enable As Boolean, RenameResourcesCont As Boolean, EncryptRes As Boolean, CompressRes As Boolean, EncryptNum As Boolean, EncryptBool As Boolean, EncryptStr As Boolean,
                       AntiIlD As Boolean, AntiT As Boolean, AntiD As Boolean, AntiDump As Boolean, HideCalls As Boolean, CtrlFlow As Boolean, InvalidM As Boolean, RenameAss As Boolean)
            _Enabled = Enable
            _RenameResourcesContent = RenameResourcesCont
            _EncryptResources = EncryptRes
            _CompressResources = CompressRes
            _EncryptNumeric = EncryptNum
            _EncryptBoolean = EncryptBool
            _EncryptString = EncryptStr
            _AntiIlDasm = AntiIlD
            _AntiTamper = AntiT
            _AntiDebug = AntiD
            _AntiDumper = AntiDump
            _HidePublicCalls = HideCalls
            _ControlFlow = CtrlFlow
            _InvalidMetadata = InvalidM
            _RenameAssembly = RenameAss
        End Sub
#End Region

#Region "IDisposable Support"
        Private disposedValue As Boolean

        Protected Overridable Sub Dispose(disposing As Boolean)
            If Not Me.disposedValue Then
                If disposing Then
                End If
                _Enabled = False
                _RenameResourcesContent = False
                _EncryptResources = False
                _CompressResources = False
                _AntiIlDasm = False
                _AntiTamper = False
                _AntiDebug = False
                _AntiDumper = False
                _EncryptBoolean = False
                _EncryptNumeric = False
                _EncryptString = False
                _HidePublicCalls = False
                _ControlFlow = False
                _InvalidMetadata = False
                _RenameAssembly = False
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
