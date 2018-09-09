port module CssCompile exposing (fileStructure, files, main)

import Css.File exposing (CssCompilerProgram, CssFileStructure)
import TreeEdit.ContextMenu.Css as ContextCss
import TreeEdit.Metadata.Css as MetadataCss
import TreeEdit.View.Css as Css


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "index.css", Css.File.compile [ Css.css, MetadataCss.css, ContextCss.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
