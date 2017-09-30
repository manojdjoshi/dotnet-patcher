Imports Mono.Cecil
Imports Helper.RandomizeHelper
Imports Helper.CecilHelper
Imports Implementer.Core.Obfuscation.Builder

Namespace Core.Obfuscation.Anti

    Public NotInheritable Class AntiDumper

#Region " Methods "
        Friend Shared Sub CreateAntiDumperClass(AssDef As AssemblyDefinition, Optional Pack As Boolean = False)

            Dim reposit = New Stub("AntiDumping", "Initialize")
            With reposit
                .ResolveTypeFromFile(GetType(AntiDumping).Assembly.Location, Finder.FindDefaultNamespace(AssDef, Pack), Randomizer.GenerateNew, Randomizer.GenerateNew)
                .InjectToCctor(AssDef)
                .DeleteDll()
            End With

        End Sub

#End Region

    End Class

End Namespace


