Namespace Engine.Context

    Public NotInheritable Class RenamedItem

#Region " Constructor "
        Friend Sub New(ItemType As RenamedItemType.ItemType, ItemName$, obfuscatedItemName$)
            _ItemType = ItemType
            _ItemName = ItemName
            _obfuscatedItemName = obfuscatedItemName
        End Sub
#End Region

#Region " Properties "
        Public ReadOnly Property ItemType As String
        Public ReadOnly Property ItemName As String
        Public ReadOnly Property obfuscatedItemName As String
#End Region

#Region " Methods "
        Private Function TypeToString(ItemType%) As String
            Select Case ItemType
                Case 0
                    Return "Namespace"
                    Exit Select
                Case 1
                    Return "Type"
                    Exit Select
                Case 2
                    Return "Method"
                    Exit Select
                Case 3
                    Return "Parameter"
                    Exit Select
                Case 4
                    Return "Generic Parameter"
                    Exit Select
                Case 5
                    Return "Variable"
                    Exit Select
                Case 6
                    Return "Property"
                    Exit Select
                Case 7
                    Return "Event"
                    Exit Select
                Case 8
                    Return "Field"
                    Exit Select
            End Select
            Return Nothing
        End Function
#End Region

    End Class
End Namespace
