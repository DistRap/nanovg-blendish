
module Graphics.NanoVG.Blendish.Icon (Icon(..), iconXY) where

data Icon =
   Icon'None
 | Icon'Question
 | Icon'Error
 | Icon'Cancel
 | Icon'TriaRight
 | Icon'TriaDown
 | Icon'TriaLeft
 | Icon'TriaUp
 | Icon'ArrowLeftright
 | Icon'Plus
 | Icon'DisclosureTriRight
 | Icon'DisclosureTriDown
 | Icon'RadiobutOff
 | Icon'RadiobutOn
 | Icon'MenuPanel
 | Icon'Blender
 | Icon'Grip
 | Icon'Dot
 | Icon'Collapsemenu
 | Icon'X
 | Icon'Duplicate
 | Icon'Trash
 | Icon'CollectionNew
 | Icon'Options
 | Icon'Node
 | Icon'NodeSel
 | Icon'Window
 | Icon'Workspace
 | Icon'RightarrowThin
 | Icon'Bordermove
 | Icon'Viewzoom
 | Icon'Add
 | Icon'Remove
 | Icon'PanelClose
 | Icon'CopyId
 | Icon'Eyedropper
 | Icon'Checkmark
 | Icon'Auto
 | Icon'CheckboxDehlt
 | Icon'CheckboxHlt
 | Icon'Unlocked
 | Icon'Locked
 | Icon'Unpinned
 | Icon'Pinned
 | Icon'ScreenBack
 | Icon'Rightarrow
 | Icon'DownarrowHlt
 | Icon'FcurveSnapshot
 | Icon'ObjectHidden
 | Icon'Topbar
 | Icon'Statusbar
 | Icon'Plugin
 | Icon'Help
 | Icon'GhostEnabled
 | Icon'Color
 | Icon'Unlinked
 | Icon'Linked
 | Icon'Hand
 | Icon'ZoomAll
 | Icon'ZoomSelected
 | Icon'ZoomPrevious
 | Icon'ZoomIn
 | Icon'ZoomOut
 | Icon'DriverDistance
 | Icon'DriverRotationalDifference
 | Icon'DriverTransform
 | Icon'Freeze
 | Icon'StylusPressure
 | Icon'GhostDisabled
 | Icon'FileNew
 | Icon'FileTick
 | Icon'Quit
 | Icon'Url
 | Icon'RecoverLast
 | Icon'ThreeDots
 | Icon'FullscreenEnter
 | Icon'FullscreenExit
 | Icon'BrushesAll
 | Icon'Light
 | Icon'Material
 | Icon'Texture
 | Icon'Anim
 | Icon'World
 | Icon'Scene
 | Icon'Output
 | Icon'8_AA
 | Icon'9_AA
 | Icon'Script
 | Icon'Particles
 | Icon'Physics
 | Icon'Speaker
 | Icon'14_AA
 | Icon'ToolSettings
 | Icon'Shaderfx
 | Icon'Modifier
 | Icon'18_AA
 | Icon'19_AA
 | Icon'20_AA
 | Icon'21_AA
 | Icon'22_AA
 | Icon'23_AA
 | Icon'Blank1
 | Icon'FakeUserOff
 | Icon'FakeUserOn
 | Icon'View3d
 | Icon'Graph
 | Icon'Outliner
 | Icon'Properties
 | Icon'Filebrowser
 | Icon'Image
 | Icon'Info
 | Icon'Sequence
 | Icon'Text
 | Icon'10_Z
 | Icon'Sound
 | Icon'Action
 | Icon'Nla
 | Icon'Preferences
 | Icon'Time
 | Icon'Nodetree
 | Icon'17_Z
 | Icon'Console
 | Icon'19_Z
 | Icon'Tracker
 | Icon'AssetManager
 | Icon'NodeCompositing
 | Icon'NodeTexture
 | Icon'NodeMaterial
 | Icon'Uv
 | Icon'26_Z
 | Icon'ObjectDatamode
 | Icon'EditmodeHlt
 | Icon'UvData
 | Icon'VpaintHlt
 | Icon'TpaintHlt
 | Icon'WpaintHlt
 | Icon'SculptmodeHlt
 | Icon'PoseHlt
 | Icon'Particlemode
 | Icon'10_Y
 | Icon'11_Y
 | Icon'12_Y
 | Icon'13_Y
 | Icon'14_Y
 | Icon'15_Y
 | Icon'16_Y
 | Icon'Tracking
 | Icon'TrackingBackwards
 | Icon'TrackingForwards
 | Icon'TrackingBackwardsSingle
 | Icon'TrackingForwardsSingle
 | Icon'TrackingClearBackwards
 | Icon'TrackingClearForwards
 | Icon'TrackingRefineBackwards
 | Icon'TrackingRefineForwards
 | Icon'26_Y
 | Icon'SceneData
 | Icon'Renderlayers
 | Icon'WorldData
 | Icon'ObjectData
 | Icon'MeshData
 | Icon'CurveData
 | Icon'MetaData
 | Icon'LatticeData
 | Icon'LightData
 | Icon'MaterialData
 | Icon'TextureData
 | Icon'AnimData
 | Icon'CameraData
 | Icon'ParticleData
 | Icon'LibraryDataDirect
 | Icon'Group
 | Icon'ArmatureData
 | Icon'Community
 | Icon'BoneData
 | Icon'Constraint
 | Icon'ShapekeyData
 | Icon'ConstraintBone
 | Icon'CameraStereo
 | Icon'Package
 | Icon'Uglypackage
 | Icon'Experimental
 | Icon'BrushData
 | Icon'ImageData
 | Icon'File
 | Icon'Fcurve
 | Icon'FontData
 | Icon'RenderResult
 | Icon'SurfaceData
 | Icon'EmptyData
 | Icon'Preset
 | Icon'RenderAnimation
 | Icon'RenderStill
 | Icon'LibraryDataBroken
 | Icon'Boids
 | Icon'Strands
 | Icon'LibraryDataIndirect
 | Icon'Greasepencil
 | Icon'LineData
 | Icon'LibraryDataOverride
 | Icon'GroupBone
 | Icon'GroupVertex
 | Icon'GroupVcol
 | Icon'GroupUvs
 | Icon'FaceMaps
 | Icon'24_W
 | Icon'Rna
 | Icon'RnaAdd
 | Icon'MouseLmb
 | Icon'MouseMmb
 | Icon'MouseRmb
 | Icon'MouseMove
 | Icon'MouseLmbDrag
 | Icon'MouseMmbDrag
 | Icon'MouseRmbDrag
 | Icon'Memory
 | Icon'PresetNew
 | Icon'10_V
 | Icon'Decorate
 | Icon'DecorateKeyframe
 | Icon'DecorateAnimate
 | Icon'DecorateDriver
 | Icon'DecorateLinked
 | Icon'DecorateLibraryOverride
 | Icon'DecorateUnlocked
 | Icon'DecorateLocked
 | Icon'DecorateOverride
 | Icon'Fund
 | Icon'TrackerData
 | Icon'Heart
 | Icon'OrphanData
 | Icon'User
 | Icon'System
 | Icon'Settings
 | Icon'OutlinerObEmpty
 | Icon'OutlinerObMesh
 | Icon'OutlinerObCurve
 | Icon'OutlinerObLattice
 | Icon'OutlinerObMeta
 | Icon'OutlinerObLight
 | Icon'OutlinerObCamera
 | Icon'OutlinerObArmature
 | Icon'OutlinerObFont
 | Icon'OutlinerObSurface
 | Icon'OutlinerObSpeaker
 | Icon'OutlinerObForceField
 | Icon'OutlinerObGroupInstance
 | Icon'OutlinerObGreasepencil
 | Icon'OutlinerObLightprobe
 | Icon'OutlinerObImage
 | Icon'17_U
 | Icon'RestrictColorOff
 | Icon'RestrictColorOn
 | Icon'HideOn
 | Icon'HideOff
 | Icon'RestrictSelectOn
 | Icon'RestrictSelectOff
 | Icon'RestrictRenderOn
 | Icon'RestrictRenderOff
 | Icon'RestrictInstancedOff
 | Icon'OutlinerDataEmpty
 | Icon'OutlinerDataMesh
 | Icon'OutlinerDataCurve
 | Icon'OutlinerDataLattice
 | Icon'OutlinerDataMeta
 | Icon'OutlinerDataLight
 | Icon'OutlinerDataCamera
 | Icon'OutlinerDataArmature
 | Icon'OutlinerDataFont
 | Icon'OutlinerDataSurface
 | Icon'OutlinerDataSpeaker
 | Icon'OutlinerDataLightprobe
 | Icon'OutlinerDataGpLayer
 | Icon'OutlinerDataGreasepencil
 | Icon'GpSelectPoints
 | Icon'GpSelectStrokes
 | Icon'GpMultiframeEditing
 | Icon'GpOnlySelected
 | Icon'GpSelectBetweenStrokes
 | Icon'ModifierOff
 | Icon'ModifierOn
 | Icon'OnionskinOff
 | Icon'OnionskinOn
 | Icon'RestrictViewOn
 | Icon'RestrictViewOff
 | Icon'RestrictInstancedOn
 | Icon'MeshPlane
 | Icon'MeshCube
 | Icon'MeshCircle
 | Icon'MeshUvsphere
 | Icon'MeshIcosphere
 | Icon'MeshGrid
 | Icon'MeshMonkey
 | Icon'MeshCylinder
 | Icon'MeshTorus
 | Icon'MeshCone
 | Icon'MeshCapsule
 | Icon'EmptySingleArrow
 | Icon'LightPoint
 | Icon'LightSun
 | Icon'LightSpot
 | Icon'LightHemi
 | Icon'LightArea
 | Icon'Cube
 | Icon'Sphere
 | Icon'Cone
 | Icon'MetaPlane
 | Icon'MetaCube
 | Icon'MetaBall
 | Icon'MetaEllipsoid
 | Icon'MetaCapsule
 | Icon'26_S
 | Icon'SurfaceNcurve
 | Icon'SurfaceNcircle
 | Icon'SurfaceNsurface
 | Icon'SurfaceNcylinder
 | Icon'SurfaceNsphere
 | Icon'SurfaceNtorus
 | Icon'EmptyAxis
 | Icon'Stroke
 | Icon'EmptyArrows
 | Icon'CurveBezcurve
 | Icon'CurveBezcircle
 | Icon'CurveNcurve
 | Icon'CurveNcircle
 | Icon'CurvePath
 | Icon'LightprobeCubemap
 | Icon'LightprobePlanar
 | Icon'LightprobeGrid
 | Icon'18_R
 | Icon'19_R
 | Icon'ColorRed
 | Icon'ColorGreen
 | Icon'ColorBlue
 | Icon'TriaRightBar
 | Icon'TriaDownBar
 | Icon'TriaLeftBar
 | Icon'TriaUpBar
 | Icon'ForceForce
 | Icon'ForceWind
 | Icon'ForceVortex
 | Icon'ForceMagnetic
 | Icon'ForceHarmonic
 | Icon'ForceCharge
 | Icon'ForceLennardjones
 | Icon'ForceTexture
 | Icon'ForceCurve
 | Icon'ForceBoid
 | Icon'ForceTurbulence
 | Icon'ForceDrag
 | Icon'ForceSmokeflow
 | Icon'14_Q
 | Icon'15_Q
 | Icon'RigidBody
 | Icon'RigidBodyConstraint
 | Icon'18_Q
 | Icon'19_Q
 | Icon'20_Q
 | Icon'21_Q
 | Icon'22_Q
 | Icon'23_Q
 | Icon'ImagePlane
 | Icon'ImageBackground
 | Icon'ImageReference
 | Icon'1_P
 | Icon'2_P
 | Icon'3_P
 | Icon'NodeInsertOn
 | Icon'NodeInsertOff
 | Icon'NodeTop
 | Icon'NodeSide
 | Icon'NodeCorner
 | Icon'AnchorTop
 | Icon'AnchorBottom
 | Icon'AnchorLeft
 | Icon'AnchorRight
 | Icon'AnchorCenter
 | Icon'14_P
 | Icon'15_P
 | Icon'16_P
 | Icon'17_P
 | Icon'18_P
 | Icon'19_P
 | Icon'20_P
 | Icon'21_P
 | Icon'SelectSet
 | Icon'SelectExtend
 | Icon'SelectSubtract
 | Icon'SelectIntersect
 | Icon'SelectDifference
 | Icon'AlignLeft
 | Icon'AlignCenter
 | Icon'AlignRight
 | Icon'AlignJustify
 | Icon'AlignFlush
 | Icon'AlignTop
 | Icon'AlignMiddle
 | Icon'AlignBottom
 | Icon'Bold
 | Icon'Italic
 | Icon'Underline
 | Icon'SmallCaps
 | Icon'13_O
 | Icon'14_O
 | Icon'ConAction
 | Icon'16_O
 | Icon'17_O
 | Icon'18_O
 | Icon'19_O
 | Icon'20_O
 | Icon'21_O
 | Icon'22_O
 | Icon'HoldoutOff
 | Icon'HoldoutOn
 | Icon'IndirectOnlyOff
 | Icon'IndirectOnlyOn
 | Icon'ConCamerasolver
 | Icon'ConFollowtrack
 | Icon'ConObjectsolver
 | Icon'ConLoclike
 | Icon'ConRotlike
 | Icon'ConSizelike
 | Icon'ConTranslike
 | Icon'ConDistlimit
 | Icon'ConLoclimit
 | Icon'ConRotlimit
 | Icon'ConSizelimit
 | Icon'ConSamevol
 | Icon'ConTransform
 | Icon'ConTransformCache
 | Icon'ConClampto
 | Icon'ConKinematic
 | Icon'ConLocktrack
 | Icon'ConSplineik
 | Icon'ConStretchto
 | Icon'ConTrackto
 | Icon'ConArmature
 | Icon'ConChildof
 | Icon'ConFloor
 | Icon'ConFollowpath
 | Icon'ConPivot
 | Icon'ConShrinkwrap
 | Icon'ModifierData
 | Icon'ModWave
 | Icon'ModBuild
 | Icon'ModDecim
 | Icon'ModMirror
 | Icon'ModSoft
 | Icon'ModSubsurf
 | Icon'Hook
 | Icon'ModPhysics
 | Icon'ModParticles
 | Icon'ModBoolean
 | Icon'ModEdgesplit
 | Icon'ModArray
 | Icon'ModUvproject
 | Icon'ModDisplace
 | Icon'ModCurve
 | Icon'ModLattice
 | Icon'ModTint
 | Icon'ModArmature
 | Icon'ModShrinkwrap
 | Icon'ModCast
 | Icon'ModMeshdeform
 | Icon'ModBevel
 | Icon'ModSmooth
 | Icon'ModSimpledeform
 | Icon'ModMask
 | Icon'ModCloth
 | Icon'ModExplode
 | Icon'ModFluidsim
 | Icon'ModMultires
 | Icon'ModFluid
 | Icon'ModSolidify
 | Icon'ModScrew
 | Icon'ModVertexWeight
 | Icon'ModDynamicpaint
 | Icon'ModRemesh
 | Icon'ModOcean
 | Icon'ModWarp
 | Icon'ModSkin
 | Icon'ModTriangulate
 | Icon'ModWireframe
 | Icon'ModDataTransfer
 | Icon'ModNormaledit
 | Icon'ModParticleInstance
 | Icon'ModHueSaturation
 | Icon'ModNoise
 | Icon'ModOffset
 | Icon'ModSimplify
 | Icon'ModThickness
 | Icon'ModInstance
 | Icon'ModTime
 | Icon'ModOpacity
 | Icon'Rec
 | Icon'Play
 | Icon'Ff
 | Icon'Rew
 | Icon'Pause
 | Icon'PrevKeyframe
 | Icon'NextKeyframe
 | Icon'PlaySound
 | Icon'PlayReverse
 | Icon'PreviewRange
 | Icon'ActionTweak
 | Icon'PmarkerAct
 | Icon'PmarkerSel
 | Icon'Pmarker
 | Icon'MarkerHlt
 | Icon'Marker
 | Icon'KeyframeHlt
 | Icon'Keyframe
 | Icon'Keyingset
 | Icon'KeyDehlt
 | Icon'KeyHlt
 | Icon'MuteIpoOff
 | Icon'MuteIpoOn
 | Icon'24_K
 | Icon'25_K
 | Icon'Driver
 | Icon'SoloOff
 | Icon'SoloOn
 | Icon'FramePrev
 | Icon'FrameNext
 | Icon'NlaPushdown
 | Icon'IpoConstant
 | Icon'IpoLinear
 | Icon'IpoBezier
 | Icon'IpoSine
 | Icon'IpoQuad
 | Icon'IpoCubic
 | Icon'IpoQuart
 | Icon'IpoQuint
 | Icon'IpoExpo
 | Icon'IpoCirc
 | Icon'IpoBounce
 | Icon'IpoElastic
 | Icon'IpoBack
 | Icon'IpoEaseIn
 | Icon'IpoEaseOut
 | Icon'IpoEaseInOut
 | Icon'NormalizeFcurves
 | Icon'23_J
 | Icon'24_J
 | Icon'25_J
 | Icon'26_J
 | Icon'Vertexsel
 | Icon'Edgesel
 | Icon'Facesel
 | Icon'4_I
 | Icon'Cursor
 | Icon'PivotBoundbox
 | Icon'PivotCursor
 | Icon'PivotIndividual
 | Icon'PivotMedian
 | Icon'PivotActive
 | Icon'CenterOnly
 | Icon'Rootcurve
 | Icon'Smoothcurve
 | Icon'Spherecurve
 | Icon'Inversesquarecurve
 | Icon'Sharpcurve
 | Icon'Lincurve
 | Icon'Nocurve
 | Icon'Rndcurve
 | Icon'PropOff
 | Icon'PropOn
 | Icon'PropCon
 | Icon'PropProjected
 | Icon'ParticlePoint
 | Icon'ParticleTip
 | Icon'ParticlePath
 | Icon'1_H
 | Icon'SnapFaceCenter
 | Icon'SnapPerpendicular
 | Icon'SnapMidpoint
 | Icon'SnapOff
 | Icon'SnapOn
 | Icon'SnapNormal
 | Icon'SnapGrid
 | Icon'SnapVertex
 | Icon'SnapEdge
 | Icon'SnapFace
 | Icon'SnapVolume
 | Icon'SnapIncrement
 | Icon'StickyUvsLoc
 | Icon'StickyUvsDisable
 | Icon'StickyUvsVert
 | Icon'ClipuvDehlt
 | Icon'ClipuvHlt
 | Icon'SnapPeelObject
 | Icon'Grid
 | Icon'ObjectOrigin
 | Icon'OrientationGlobal
 | Icon'OrientationGimbal
 | Icon'OrientationLocal
 | Icon'OrientationNormal
 | Icon'OrientationView
 | Icon'Copydown
 | Icon'Pastedown
 | Icon'Pasteflipup
 | Icon'Pasteflipdown
 | Icon'VisSel11
 | Icon'VisSel10
 | Icon'VisSel01
 | Icon'VisSel00
 | Icon'9_G
 | Icon'AutomergeOff
 | Icon'AutomergeOn
 | Icon'12_G
 | Icon'UvVertexsel
 | Icon'UvEdgesel
 | Icon'UvFacesel
 | Icon'UvIslandsel
 | Icon'UvSyncSelect
 | Icon'18_G
 | Icon'19_G
 | Icon'20_G
 | Icon'TransformOrigins
 | Icon'Gizmo
 | Icon'OrientationCursor
 | Icon'NormalsVertex
 | Icon'NormalsFace
 | Icon'NormalsVertexFace
 | Icon'ShadingBbox
 | Icon'ShadingWire
 | Icon'ShadingSolid
 | Icon'ShadingRendered
 | Icon'ShadingTexture
 | Icon'Overlay
 | Icon'Xray
 | Icon'8_F
 | Icon'9_F
 | Icon'LockviewOff
 | Icon'LockviewOn
 | Icon'12_F
 | Icon'AxisSide
 | Icon'AxisFront
 | Icon'AxisTop
 | Icon'16_F
 | Icon'17_F
 | Icon'18_F
 | Icon'19_F
 | Icon'LayerUsed
 | Icon'LayerActive
 | Icon'22_F
 | Icon'23_F
 | Icon'24_F
 | Icon'25_F
 | Icon'26_F
 | Icon'1_E
 | Icon'2_E
 | Icon'3_E
 | Icon'4_E
 | Icon'5_E
 | Icon'6_E
 | Icon'7_E
 | Icon'8_E
 | Icon'9_E
 | Icon'10_E
 | Icon'11_E
 | Icon'12_E
 | Icon'13_E
 | Icon'14_E
 | Icon'15_E
 | Icon'16_E
 | Icon'17_E
 | Icon'18_E
 | Icon'19_E
 | Icon'20_E
 | Icon'21_E
 | Icon'22_E
 | Icon'23_E
 | Icon'Home
 | Icon'Documents
 | Icon'Temp
 | Icon'Sortalpha
 | Icon'Sortbyext
 | Icon'Sorttime
 | Icon'Sortsize
 | Icon'Shortdisplay
 | Icon'Longdisplay
 | Icon'7_D
 | Icon'Imgdisplay
 | Icon'9_D
 | Icon'10_D
 | Icon'Bookmarks
 | Icon'Fontpreview
 | Icon'Filter
 | Icon'Newfolder
 | Icon'FolderRedirect
 | Icon'FileParent
 | Icon'FileRefresh
 | Icon'FileFolder
 | Icon'FileBlank
 | Icon'FileBlend
 | Icon'FileImage
 | Icon'FileMovie
 | Icon'FileScript
 | Icon'FileSound
 | Icon'FileFont
 | Icon'FileText
 | Icon'SortDesc
 | Icon'SortAsc
 | Icon'LinkBlend
 | Icon'AppendBlend
 | Icon'Import
 | Icon'Export
 | Icon'7_C
 | Icon'8_C
 | Icon'9_C
 | Icon'10_C
 | Icon'11_C
 | Icon'12_C
 | Icon'13_C
 | Icon'LoopBack
 | Icon'LoopForwards
 | Icon'Back
 | Icon'Forward
 | Icon'18_C
 | Icon'19_C
 | Icon'FileArchive
 | Icon'FileCache
 | Icon'FileVolume
 | Icon'File3d
 | Icon'FileHidden
 | Icon'FileBackup
 | Icon'DiskDrive
 | Icon'Matplane
 | Icon'Matsphere
 | Icon'Matcube
 | Icon'Monkey
 | Icon'Hair
 | Icon'Aliased
 | Icon'Antialiased
 | Icon'MatSphereSky
 | Icon'Matshaderball
 | Icon'Matcloth
 | Icon'Matfluid
 | Icon'12_B
 | Icon'WordwrapOff
 | Icon'WordwrapOn
 | Icon'SyntaxOff
 | Icon'SyntaxOn
 | Icon'LinenumbersOff
 | Icon'LinenumbersOn
 | Icon'Scriptplugins
 | Icon'20_B
 | Icon'21_B
 | Icon'22_B
 | Icon'Disc
 | Icon'Desktop
 | Icon'ExternalDrive
 | Icon'NetworkDrive
 | Icon'SeqSequencer
 | Icon'SeqPreview
 | Icon'SeqLumaWaveform
 | Icon'SeqChromaScope
 | Icon'SeqHistogram
 | Icon'SeqSplitview
 | Icon'SeqStripMeta
 | Icon'SeqStripDuplicate
 | Icon'9_A
 | Icon'ImageRgb
 | Icon'ImageRgbAlpha
 | Icon'ImageAlpha
 | Icon'ImageZdepth
 | Icon'HandleAutoclamped
 | Icon'HandleAuto
 | Icon'HandleAligned
 | Icon'HandleVector
 | Icon'HandleFree
 | Icon'19_A
 | Icon'20_A
 | Icon'21_A
 | Icon'ViewPerspective
 | Icon'ViewOrtho
 | Icon'ViewCamera
 | Icon'ViewPan
 | Icon'ViewZoom
 deriving (Eq, Show, Ord)

iconXY :: Icon -> (Integer, Integer)
iconXY Icon'None = (0, 29)
iconXY Icon'Question = (1, 29)
iconXY Icon'Error = (2, 29)
iconXY Icon'Cancel = (3, 29)
iconXY Icon'TriaRight = (4, 29)
iconXY Icon'TriaDown = (5, 29)
iconXY Icon'TriaLeft = (6, 29)
iconXY Icon'TriaUp = (7, 29)
iconXY Icon'ArrowLeftright = (8, 29)
iconXY Icon'Plus = (9, 29)
iconXY Icon'DisclosureTriRight = (10, 29)
iconXY Icon'DisclosureTriDown = (11, 29)
iconXY Icon'RadiobutOff = (12, 29)
iconXY Icon'RadiobutOn = (13, 29)
iconXY Icon'MenuPanel = (14, 29)
iconXY Icon'Blender = (15, 29)
iconXY Icon'Grip = (16, 29)
iconXY Icon'Dot = (17, 29)
iconXY Icon'Collapsemenu = (18, 29)
iconXY Icon'X = (19, 29)
iconXY Icon'Duplicate = (20, 29)
iconXY Icon'Trash = (21, 29)
iconXY Icon'CollectionNew = (22, 29)
iconXY Icon'Options = (23, 29)
iconXY Icon'Node = (24, 29)
iconXY Icon'NodeSel = (25, 29)
iconXY Icon'Window = (0, 28)
iconXY Icon'Workspace = (1, 28)
iconXY Icon'RightarrowThin = (2, 28)
iconXY Icon'Bordermove = (3, 28)
iconXY Icon'Viewzoom = (4, 28)
iconXY Icon'Add = (5, 28)
iconXY Icon'Remove = (6, 28)
iconXY Icon'PanelClose = (7, 28)
iconXY Icon'CopyId = (8, 28)
iconXY Icon'Eyedropper = (9, 28)
iconXY Icon'Checkmark = (10, 28)
iconXY Icon'Auto = (11, 28)
iconXY Icon'CheckboxDehlt = (12, 28)
iconXY Icon'CheckboxHlt = (13, 28)
iconXY Icon'Unlocked = (14, 28)
iconXY Icon'Locked = (15, 28)
iconXY Icon'Unpinned = (16, 28)
iconXY Icon'Pinned = (17, 28)
iconXY Icon'ScreenBack = (18, 28)
iconXY Icon'Rightarrow = (19, 28)
iconXY Icon'DownarrowHlt = (20, 28)
iconXY Icon'FcurveSnapshot = (21, 28)
iconXY Icon'ObjectHidden = (22, 28)
iconXY Icon'Topbar = (23, 28)
iconXY Icon'Statusbar = (24, 28)
iconXY Icon'Plugin = (25, 28)
iconXY Icon'Help = (0, 27)
iconXY Icon'GhostEnabled = (1, 27)
iconXY Icon'Color = (2, 27)
iconXY Icon'Unlinked = (3, 27)
iconXY Icon'Linked = (4, 27)
iconXY Icon'Hand = (5, 27)
iconXY Icon'ZoomAll = (6, 27)
iconXY Icon'ZoomSelected = (7, 27)
iconXY Icon'ZoomPrevious = (8, 27)
iconXY Icon'ZoomIn = (9, 27)
iconXY Icon'ZoomOut = (10, 27)
iconXY Icon'DriverDistance = (11, 27)
iconXY Icon'DriverRotationalDifference = (12, 27)
iconXY Icon'DriverTransform = (13, 27)
iconXY Icon'Freeze = (14, 27)
iconXY Icon'StylusPressure = (15, 27)
iconXY Icon'GhostDisabled = (16, 27)
iconXY Icon'FileNew = (17, 27)
iconXY Icon'FileTick = (18, 27)
iconXY Icon'Quit = (19, 27)
iconXY Icon'Url = (20, 27)
iconXY Icon'RecoverLast = (21, 27)
iconXY Icon'ThreeDots = (22, 27)
iconXY Icon'FullscreenEnter = (23, 27)
iconXY Icon'FullscreenExit = (24, 27)
iconXY Icon'BrushesAll = (25, 27)
iconXY Icon'Light = (0, 26)
iconXY Icon'Material = (1, 26)
iconXY Icon'Texture = (2, 26)
iconXY Icon'Anim = (3, 26)
iconXY Icon'World = (4, 26)
iconXY Icon'Scene = (5, 26)
iconXY Icon'Output = (6, 26)
iconXY Icon'8_AA = (7, 26)
iconXY Icon'9_AA = (8, 26)
iconXY Icon'Script = (9, 26)
iconXY Icon'Particles = (10, 26)
iconXY Icon'Physics = (11, 26)
iconXY Icon'Speaker = (12, 26)
iconXY Icon'14_AA = (13, 26)
iconXY Icon'ToolSettings = (14, 26)
iconXY Icon'Shaderfx = (15, 26)
iconXY Icon'Modifier = (16, 26)
iconXY Icon'18_AA = (17, 26)
iconXY Icon'19_AA = (18, 26)
iconXY Icon'20_AA = (19, 26)
iconXY Icon'21_AA = (20, 26)
iconXY Icon'22_AA = (21, 26)
iconXY Icon'23_AA = (22, 26)
iconXY Icon'Blank1 = (23, 26)
iconXY Icon'FakeUserOff = (24, 26)
iconXY Icon'FakeUserOn = (25, 26)
iconXY Icon'View3d = (0, 25)
iconXY Icon'Graph = (1, 25)
iconXY Icon'Outliner = (2, 25)
iconXY Icon'Properties = (3, 25)
iconXY Icon'Filebrowser = (4, 25)
iconXY Icon'Image = (5, 25)
iconXY Icon'Info = (6, 25)
iconXY Icon'Sequence = (7, 25)
iconXY Icon'Text = (8, 25)
iconXY Icon'10_Z = (9, 25)
iconXY Icon'Sound = (10, 25)
iconXY Icon'Action = (11, 25)
iconXY Icon'Nla = (12, 25)
iconXY Icon'Preferences = (13, 25)
iconXY Icon'Time = (14, 25)
iconXY Icon'Nodetree = (15, 25)
iconXY Icon'17_Z = (16, 25)
iconXY Icon'Console = (17, 25)
iconXY Icon'19_Z = (18, 25)
iconXY Icon'Tracker = (19, 25)
iconXY Icon'AssetManager = (20, 25)
iconXY Icon'NodeCompositing = (21, 25)
iconXY Icon'NodeTexture = (22, 25)
iconXY Icon'NodeMaterial = (23, 25)
iconXY Icon'Uv = (24, 25)
iconXY Icon'26_Z = (25, 25)
iconXY Icon'ObjectDatamode = (0, 24)
iconXY Icon'EditmodeHlt = (1, 24)
iconXY Icon'UvData = (2, 24)
iconXY Icon'VpaintHlt = (3, 24)
iconXY Icon'TpaintHlt = (4, 24)
iconXY Icon'WpaintHlt = (5, 24)
iconXY Icon'SculptmodeHlt = (6, 24)
iconXY Icon'PoseHlt = (7, 24)
iconXY Icon'Particlemode = (8, 24)
iconXY Icon'10_Y = (9, 24)
iconXY Icon'11_Y = (10, 24)
iconXY Icon'12_Y = (11, 24)
iconXY Icon'13_Y = (12, 24)
iconXY Icon'14_Y = (13, 24)
iconXY Icon'15_Y = (14, 24)
iconXY Icon'16_Y = (15, 24)
iconXY Icon'Tracking = (16, 24)
iconXY Icon'TrackingBackwards = (17, 24)
iconXY Icon'TrackingForwards = (18, 24)
iconXY Icon'TrackingBackwardsSingle = (19, 24)
iconXY Icon'TrackingForwardsSingle = (20, 24)
iconXY Icon'TrackingClearBackwards = (21, 24)
iconXY Icon'TrackingClearForwards = (22, 24)
iconXY Icon'TrackingRefineBackwards = (23, 24)
iconXY Icon'TrackingRefineForwards = (24, 24)
iconXY Icon'26_Y = (25, 24)
iconXY Icon'SceneData = (0, 23)
iconXY Icon'Renderlayers = (1, 23)
iconXY Icon'WorldData = (2, 23)
iconXY Icon'ObjectData = (3, 23)
iconXY Icon'MeshData = (4, 23)
iconXY Icon'CurveData = (5, 23)
iconXY Icon'MetaData = (6, 23)
iconXY Icon'LatticeData = (7, 23)
iconXY Icon'LightData = (8, 23)
iconXY Icon'MaterialData = (9, 23)
iconXY Icon'TextureData = (10, 23)
iconXY Icon'AnimData = (11, 23)
iconXY Icon'CameraData = (12, 23)
iconXY Icon'ParticleData = (13, 23)
iconXY Icon'LibraryDataDirect = (14, 23)
iconXY Icon'Group = (15, 23)
iconXY Icon'ArmatureData = (16, 23)
iconXY Icon'Community = (17, 23)
iconXY Icon'BoneData = (18, 23)
iconXY Icon'Constraint = (19, 23)
iconXY Icon'ShapekeyData = (20, 23)
iconXY Icon'ConstraintBone = (21, 23)
iconXY Icon'CameraStereo = (22, 23)
iconXY Icon'Package = (23, 23)
iconXY Icon'Uglypackage = (24, 23)
iconXY Icon'Experimental = (25, 23)
iconXY Icon'BrushData = (0, 22)
iconXY Icon'ImageData = (1, 22)
iconXY Icon'File = (2, 22)
iconXY Icon'Fcurve = (3, 22)
iconXY Icon'FontData = (4, 22)
iconXY Icon'RenderResult = (5, 22)
iconXY Icon'SurfaceData = (6, 22)
iconXY Icon'EmptyData = (7, 22)
iconXY Icon'Preset = (8, 22)
iconXY Icon'RenderAnimation = (9, 22)
iconXY Icon'RenderStill = (10, 22)
iconXY Icon'LibraryDataBroken = (11, 22)
iconXY Icon'Boids = (12, 22)
iconXY Icon'Strands = (13, 22)
iconXY Icon'LibraryDataIndirect = (14, 22)
iconXY Icon'Greasepencil = (15, 22)
iconXY Icon'LineData = (16, 22)
iconXY Icon'LibraryDataOverride = (17, 22)
iconXY Icon'GroupBone = (18, 22)
iconXY Icon'GroupVertex = (19, 22)
iconXY Icon'GroupVcol = (20, 22)
iconXY Icon'GroupUvs = (21, 22)
iconXY Icon'FaceMaps = (22, 22)
iconXY Icon'24_W = (23, 22)
iconXY Icon'Rna = (24, 22)
iconXY Icon'RnaAdd = (25, 22)
iconXY Icon'MouseLmb = (0, 21)
iconXY Icon'MouseMmb = (1, 21)
iconXY Icon'MouseRmb = (2, 21)
iconXY Icon'MouseMove = (3, 21)
iconXY Icon'MouseLmbDrag = (4, 21)
iconXY Icon'MouseMmbDrag = (5, 21)
iconXY Icon'MouseRmbDrag = (6, 21)
iconXY Icon'Memory = (7, 21)
iconXY Icon'PresetNew = (8, 21)
iconXY Icon'10_V = (9, 21)
iconXY Icon'Decorate = (10, 21)
iconXY Icon'DecorateKeyframe = (11, 21)
iconXY Icon'DecorateAnimate = (12, 21)
iconXY Icon'DecorateDriver = (13, 21)
iconXY Icon'DecorateLinked = (14, 21)
iconXY Icon'DecorateLibraryOverride = (15, 21)
iconXY Icon'DecorateUnlocked = (16, 21)
iconXY Icon'DecorateLocked = (17, 21)
iconXY Icon'DecorateOverride = (18, 21)
iconXY Icon'Fund = (19, 21)
iconXY Icon'TrackerData = (20, 21)
iconXY Icon'Heart = (21, 21)
iconXY Icon'OrphanData = (22, 21)
iconXY Icon'User = (23, 21)
iconXY Icon'System = (24, 21)
iconXY Icon'Settings = (25, 21)
iconXY Icon'OutlinerObEmpty = (0, 20)
iconXY Icon'OutlinerObMesh = (1, 20)
iconXY Icon'OutlinerObCurve = (2, 20)
iconXY Icon'OutlinerObLattice = (3, 20)
iconXY Icon'OutlinerObMeta = (4, 20)
iconXY Icon'OutlinerObLight = (5, 20)
iconXY Icon'OutlinerObCamera = (6, 20)
iconXY Icon'OutlinerObArmature = (7, 20)
iconXY Icon'OutlinerObFont = (8, 20)
iconXY Icon'OutlinerObSurface = (9, 20)
iconXY Icon'OutlinerObSpeaker = (10, 20)
iconXY Icon'OutlinerObForceField = (11, 20)
iconXY Icon'OutlinerObGroupInstance = (12, 20)
iconXY Icon'OutlinerObGreasepencil = (13, 20)
iconXY Icon'OutlinerObLightprobe = (14, 20)
iconXY Icon'OutlinerObImage = (15, 20)
iconXY Icon'17_U = (16, 20)
iconXY Icon'RestrictColorOff = (17, 20)
iconXY Icon'RestrictColorOn = (18, 20)
iconXY Icon'HideOn = (19, 20)
iconXY Icon'HideOff = (20, 20)
iconXY Icon'RestrictSelectOn = (21, 20)
iconXY Icon'RestrictSelectOff = (22, 20)
iconXY Icon'RestrictRenderOn = (23, 20)
iconXY Icon'RestrictRenderOff = (24, 20)
iconXY Icon'RestrictInstancedOff = (25, 20)
iconXY Icon'OutlinerDataEmpty = (0, 19)
iconXY Icon'OutlinerDataMesh = (1, 19)
iconXY Icon'OutlinerDataCurve = (2, 19)
iconXY Icon'OutlinerDataLattice = (3, 19)
iconXY Icon'OutlinerDataMeta = (4, 19)
iconXY Icon'OutlinerDataLight = (5, 19)
iconXY Icon'OutlinerDataCamera = (6, 19)
iconXY Icon'OutlinerDataArmature = (7, 19)
iconXY Icon'OutlinerDataFont = (8, 19)
iconXY Icon'OutlinerDataSurface = (9, 19)
iconXY Icon'OutlinerDataSpeaker = (10, 19)
iconXY Icon'OutlinerDataLightprobe = (11, 19)
iconXY Icon'OutlinerDataGpLayer = (12, 19)
iconXY Icon'OutlinerDataGreasepencil = (13, 19)
iconXY Icon'GpSelectPoints = (14, 19)
iconXY Icon'GpSelectStrokes = (15, 19)
iconXY Icon'GpMultiframeEditing = (16, 19)
iconXY Icon'GpOnlySelected = (17, 19)
iconXY Icon'GpSelectBetweenStrokes = (18, 19)
iconXY Icon'ModifierOff = (19, 19)
iconXY Icon'ModifierOn = (20, 19)
iconXY Icon'OnionskinOff = (21, 19)
iconXY Icon'OnionskinOn = (22, 19)
iconXY Icon'RestrictViewOn = (23, 19)
iconXY Icon'RestrictViewOff = (24, 19)
iconXY Icon'RestrictInstancedOn = (25, 19)
iconXY Icon'MeshPlane = (0, 18)
iconXY Icon'MeshCube = (1, 18)
iconXY Icon'MeshCircle = (2, 18)
iconXY Icon'MeshUvsphere = (3, 18)
iconXY Icon'MeshIcosphere = (4, 18)
iconXY Icon'MeshGrid = (5, 18)
iconXY Icon'MeshMonkey = (6, 18)
iconXY Icon'MeshCylinder = (7, 18)
iconXY Icon'MeshTorus = (8, 18)
iconXY Icon'MeshCone = (9, 18)
iconXY Icon'MeshCapsule = (10, 18)
iconXY Icon'EmptySingleArrow = (11, 18)
iconXY Icon'LightPoint = (12, 18)
iconXY Icon'LightSun = (13, 18)
iconXY Icon'LightSpot = (14, 18)
iconXY Icon'LightHemi = (15, 18)
iconXY Icon'LightArea = (16, 18)
iconXY Icon'Cube = (17, 18)
iconXY Icon'Sphere = (18, 18)
iconXY Icon'Cone = (19, 18)
iconXY Icon'MetaPlane = (20, 18)
iconXY Icon'MetaCube = (21, 18)
iconXY Icon'MetaBall = (22, 18)
iconXY Icon'MetaEllipsoid = (23, 18)
iconXY Icon'MetaCapsule = (24, 18)
iconXY Icon'26_S = (25, 18)
iconXY Icon'SurfaceNcurve = (0, 17)
iconXY Icon'SurfaceNcircle = (1, 17)
iconXY Icon'SurfaceNsurface = (2, 17)
iconXY Icon'SurfaceNcylinder = (3, 17)
iconXY Icon'SurfaceNsphere = (4, 17)
iconXY Icon'SurfaceNtorus = (5, 17)
iconXY Icon'EmptyAxis = (6, 17)
iconXY Icon'Stroke = (7, 17)
iconXY Icon'EmptyArrows = (8, 17)
iconXY Icon'CurveBezcurve = (9, 17)
iconXY Icon'CurveBezcircle = (10, 17)
iconXY Icon'CurveNcurve = (11, 17)
iconXY Icon'CurveNcircle = (12, 17)
iconXY Icon'CurvePath = (13, 17)
iconXY Icon'LightprobeCubemap = (14, 17)
iconXY Icon'LightprobePlanar = (15, 17)
iconXY Icon'LightprobeGrid = (16, 17)
iconXY Icon'18_R = (17, 17)
iconXY Icon'19_R = (18, 17)
iconXY Icon'ColorRed = (19, 17)
iconXY Icon'ColorGreen = (20, 17)
iconXY Icon'ColorBlue = (21, 17)
iconXY Icon'TriaRightBar = (22, 17)
iconXY Icon'TriaDownBar = (23, 17)
iconXY Icon'TriaLeftBar = (24, 17)
iconXY Icon'TriaUpBar = (25, 17)
iconXY Icon'ForceForce = (0, 16)
iconXY Icon'ForceWind = (1, 16)
iconXY Icon'ForceVortex = (2, 16)
iconXY Icon'ForceMagnetic = (3, 16)
iconXY Icon'ForceHarmonic = (4, 16)
iconXY Icon'ForceCharge = (5, 16)
iconXY Icon'ForceLennardjones = (6, 16)
iconXY Icon'ForceTexture = (7, 16)
iconXY Icon'ForceCurve = (8, 16)
iconXY Icon'ForceBoid = (9, 16)
iconXY Icon'ForceTurbulence = (10, 16)
iconXY Icon'ForceDrag = (11, 16)
iconXY Icon'ForceSmokeflow = (12, 16)
iconXY Icon'14_Q = (13, 16)
iconXY Icon'15_Q = (14, 16)
iconXY Icon'RigidBody = (15, 16)
iconXY Icon'RigidBodyConstraint = (16, 16)
iconXY Icon'18_Q = (17, 16)
iconXY Icon'19_Q = (18, 16)
iconXY Icon'20_Q = (19, 16)
iconXY Icon'21_Q = (20, 16)
iconXY Icon'22_Q = (21, 16)
iconXY Icon'23_Q = (22, 16)
iconXY Icon'ImagePlane = (23, 16)
iconXY Icon'ImageBackground = (24, 16)
iconXY Icon'ImageReference = (25, 16)
iconXY Icon'1_P = (0, 15)
iconXY Icon'2_P = (1, 15)
iconXY Icon'3_P = (2, 15)
iconXY Icon'NodeInsertOn = (3, 15)
iconXY Icon'NodeInsertOff = (4, 15)
iconXY Icon'NodeTop = (5, 15)
iconXY Icon'NodeSide = (6, 15)
iconXY Icon'NodeCorner = (7, 15)
iconXY Icon'AnchorTop = (8, 15)
iconXY Icon'AnchorBottom = (9, 15)
iconXY Icon'AnchorLeft = (10, 15)
iconXY Icon'AnchorRight = (11, 15)
iconXY Icon'AnchorCenter = (12, 15)
iconXY Icon'14_P = (13, 15)
iconXY Icon'15_P = (14, 15)
iconXY Icon'16_P = (15, 15)
iconXY Icon'17_P = (16, 15)
iconXY Icon'18_P = (17, 15)
iconXY Icon'19_P = (18, 15)
iconXY Icon'20_P = (19, 15)
iconXY Icon'21_P = (20, 15)
iconXY Icon'SelectSet = (21, 15)
iconXY Icon'SelectExtend = (22, 15)
iconXY Icon'SelectSubtract = (23, 15)
iconXY Icon'SelectIntersect = (24, 15)
iconXY Icon'SelectDifference = (25, 15)
iconXY Icon'AlignLeft = (0, 14)
iconXY Icon'AlignCenter = (1, 14)
iconXY Icon'AlignRight = (2, 14)
iconXY Icon'AlignJustify = (3, 14)
iconXY Icon'AlignFlush = (4, 14)
iconXY Icon'AlignTop = (5, 14)
iconXY Icon'AlignMiddle = (6, 14)
iconXY Icon'AlignBottom = (7, 14)
iconXY Icon'Bold = (8, 14)
iconXY Icon'Italic = (9, 14)
iconXY Icon'Underline = (10, 14)
iconXY Icon'SmallCaps = (11, 14)
iconXY Icon'13_O = (12, 14)
iconXY Icon'14_O = (13, 14)
iconXY Icon'ConAction = (14, 14)
iconXY Icon'16_O = (15, 14)
iconXY Icon'17_O = (16, 14)
iconXY Icon'18_O = (17, 14)
iconXY Icon'19_O = (18, 14)
iconXY Icon'20_O = (19, 14)
iconXY Icon'21_O = (20, 14)
iconXY Icon'22_O = (21, 14)
iconXY Icon'HoldoutOff = (22, 14)
iconXY Icon'HoldoutOn = (23, 14)
iconXY Icon'IndirectOnlyOff = (24, 14)
iconXY Icon'IndirectOnlyOn = (25, 14)
iconXY Icon'ConCamerasolver = (0, 13)
iconXY Icon'ConFollowtrack = (1, 13)
iconXY Icon'ConObjectsolver = (2, 13)
iconXY Icon'ConLoclike = (3, 13)
iconXY Icon'ConRotlike = (4, 13)
iconXY Icon'ConSizelike = (5, 13)
iconXY Icon'ConTranslike = (6, 13)
iconXY Icon'ConDistlimit = (7, 13)
iconXY Icon'ConLoclimit = (8, 13)
iconXY Icon'ConRotlimit = (9, 13)
iconXY Icon'ConSizelimit = (10, 13)
iconXY Icon'ConSamevol = (11, 13)
iconXY Icon'ConTransform = (12, 13)
iconXY Icon'ConTransformCache = (13, 13)
iconXY Icon'ConClampto = (14, 13)
iconXY Icon'ConKinematic = (15, 13)
iconXY Icon'ConLocktrack = (16, 13)
iconXY Icon'ConSplineik = (17, 13)
iconXY Icon'ConStretchto = (18, 13)
iconXY Icon'ConTrackto = (19, 13)
iconXY Icon'ConArmature = (20, 13)
iconXY Icon'ConChildof = (21, 13)
iconXY Icon'ConFloor = (22, 13)
iconXY Icon'ConFollowpath = (23, 13)
iconXY Icon'ConPivot = (24, 13)
iconXY Icon'ConShrinkwrap = (25, 13)
iconXY Icon'ModifierData = (0, 12)
iconXY Icon'ModWave = (1, 12)
iconXY Icon'ModBuild = (2, 12)
iconXY Icon'ModDecim = (3, 12)
iconXY Icon'ModMirror = (4, 12)
iconXY Icon'ModSoft = (5, 12)
iconXY Icon'ModSubsurf = (6, 12)
iconXY Icon'Hook = (7, 12)
iconXY Icon'ModPhysics = (8, 12)
iconXY Icon'ModParticles = (9, 12)
iconXY Icon'ModBoolean = (10, 12)
iconXY Icon'ModEdgesplit = (11, 12)
iconXY Icon'ModArray = (12, 12)
iconXY Icon'ModUvproject = (13, 12)
iconXY Icon'ModDisplace = (14, 12)
iconXY Icon'ModCurve = (15, 12)
iconXY Icon'ModLattice = (16, 12)
iconXY Icon'ModTint = (17, 12)
iconXY Icon'ModArmature = (18, 12)
iconXY Icon'ModShrinkwrap = (19, 12)
iconXY Icon'ModCast = (20, 12)
iconXY Icon'ModMeshdeform = (21, 12)
iconXY Icon'ModBevel = (22, 12)
iconXY Icon'ModSmooth = (23, 12)
iconXY Icon'ModSimpledeform = (24, 12)
iconXY Icon'ModMask = (25, 12)
iconXY Icon'ModCloth = (0, 11)
iconXY Icon'ModExplode = (1, 11)
iconXY Icon'ModFluidsim = (2, 11)
iconXY Icon'ModMultires = (3, 11)
iconXY Icon'ModFluid = (4, 11)
iconXY Icon'ModSolidify = (5, 11)
iconXY Icon'ModScrew = (6, 11)
iconXY Icon'ModVertexWeight = (7, 11)
iconXY Icon'ModDynamicpaint = (8, 11)
iconXY Icon'ModRemesh = (9, 11)
iconXY Icon'ModOcean = (10, 11)
iconXY Icon'ModWarp = (11, 11)
iconXY Icon'ModSkin = (12, 11)
iconXY Icon'ModTriangulate = (13, 11)
iconXY Icon'ModWireframe = (14, 11)
iconXY Icon'ModDataTransfer = (15, 11)
iconXY Icon'ModNormaledit = (16, 11)
iconXY Icon'ModParticleInstance = (17, 11)
iconXY Icon'ModHueSaturation = (18, 11)
iconXY Icon'ModNoise = (19, 11)
iconXY Icon'ModOffset = (20, 11)
iconXY Icon'ModSimplify = (21, 11)
iconXY Icon'ModThickness = (22, 11)
iconXY Icon'ModInstance = (23, 11)
iconXY Icon'ModTime = (24, 11)
iconXY Icon'ModOpacity = (25, 11)
iconXY Icon'Rec = (0, 10)
iconXY Icon'Play = (1, 10)
iconXY Icon'Ff = (2, 10)
iconXY Icon'Rew = (3, 10)
iconXY Icon'Pause = (4, 10)
iconXY Icon'PrevKeyframe = (5, 10)
iconXY Icon'NextKeyframe = (6, 10)
iconXY Icon'PlaySound = (7, 10)
iconXY Icon'PlayReverse = (8, 10)
iconXY Icon'PreviewRange = (9, 10)
iconXY Icon'ActionTweak = (10, 10)
iconXY Icon'PmarkerAct = (11, 10)
iconXY Icon'PmarkerSel = (12, 10)
iconXY Icon'Pmarker = (13, 10)
iconXY Icon'MarkerHlt = (14, 10)
iconXY Icon'Marker = (15, 10)
iconXY Icon'KeyframeHlt = (16, 10)
iconXY Icon'Keyframe = (17, 10)
iconXY Icon'Keyingset = (18, 10)
iconXY Icon'KeyDehlt = (19, 10)
iconXY Icon'KeyHlt = (20, 10)
iconXY Icon'MuteIpoOff = (21, 10)
iconXY Icon'MuteIpoOn = (22, 10)
iconXY Icon'24_K = (23, 10)
iconXY Icon'25_K = (24, 10)
iconXY Icon'Driver = (25, 10)
iconXY Icon'SoloOff = (0, 9)
iconXY Icon'SoloOn = (1, 9)
iconXY Icon'FramePrev = (2, 9)
iconXY Icon'FrameNext = (3, 9)
iconXY Icon'NlaPushdown = (4, 9)
iconXY Icon'IpoConstant = (5, 9)
iconXY Icon'IpoLinear = (6, 9)
iconXY Icon'IpoBezier = (7, 9)
iconXY Icon'IpoSine = (8, 9)
iconXY Icon'IpoQuad = (9, 9)
iconXY Icon'IpoCubic = (10, 9)
iconXY Icon'IpoQuart = (11, 9)
iconXY Icon'IpoQuint = (12, 9)
iconXY Icon'IpoExpo = (13, 9)
iconXY Icon'IpoCirc = (14, 9)
iconXY Icon'IpoBounce = (15, 9)
iconXY Icon'IpoElastic = (16, 9)
iconXY Icon'IpoBack = (17, 9)
iconXY Icon'IpoEaseIn = (18, 9)
iconXY Icon'IpoEaseOut = (19, 9)
iconXY Icon'IpoEaseInOut = (20, 9)
iconXY Icon'NormalizeFcurves = (21, 9)
iconXY Icon'23_J = (22, 9)
iconXY Icon'24_J = (23, 9)
iconXY Icon'25_J = (24, 9)
iconXY Icon'26_J = (25, 9)
iconXY Icon'Vertexsel = (0, 8)
iconXY Icon'Edgesel = (1, 8)
iconXY Icon'Facesel = (2, 8)
iconXY Icon'4_I = (3, 8)
iconXY Icon'Cursor = (4, 8)
iconXY Icon'PivotBoundbox = (5, 8)
iconXY Icon'PivotCursor = (6, 8)
iconXY Icon'PivotIndividual = (7, 8)
iconXY Icon'PivotMedian = (8, 8)
iconXY Icon'PivotActive = (9, 8)
iconXY Icon'CenterOnly = (10, 8)
iconXY Icon'Rootcurve = (11, 8)
iconXY Icon'Smoothcurve = (12, 8)
iconXY Icon'Spherecurve = (13, 8)
iconXY Icon'Inversesquarecurve = (14, 8)
iconXY Icon'Sharpcurve = (15, 8)
iconXY Icon'Lincurve = (16, 8)
iconXY Icon'Nocurve = (17, 8)
iconXY Icon'Rndcurve = (18, 8)
iconXY Icon'PropOff = (19, 8)
iconXY Icon'PropOn = (20, 8)
iconXY Icon'PropCon = (21, 8)
iconXY Icon'PropProjected = (22, 8)
iconXY Icon'ParticlePoint = (23, 8)
iconXY Icon'ParticleTip = (24, 8)
iconXY Icon'ParticlePath = (25, 8)
iconXY Icon'1_H = (0, 7)
iconXY Icon'SnapFaceCenter = (1, 7)
iconXY Icon'SnapPerpendicular = (2, 7)
iconXY Icon'SnapMidpoint = (3, 7)
iconXY Icon'SnapOff = (4, 7)
iconXY Icon'SnapOn = (5, 7)
iconXY Icon'SnapNormal = (6, 7)
iconXY Icon'SnapGrid = (7, 7)
iconXY Icon'SnapVertex = (8, 7)
iconXY Icon'SnapEdge = (9, 7)
iconXY Icon'SnapFace = (10, 7)
iconXY Icon'SnapVolume = (11, 7)
iconXY Icon'SnapIncrement = (12, 7)
iconXY Icon'StickyUvsLoc = (13, 7)
iconXY Icon'StickyUvsDisable = (14, 7)
iconXY Icon'StickyUvsVert = (15, 7)
iconXY Icon'ClipuvDehlt = (16, 7)
iconXY Icon'ClipuvHlt = (17, 7)
iconXY Icon'SnapPeelObject = (18, 7)
iconXY Icon'Grid = (19, 7)
iconXY Icon'ObjectOrigin = (20, 7)
iconXY Icon'OrientationGlobal = (21, 7)
iconXY Icon'OrientationGimbal = (22, 7)
iconXY Icon'OrientationLocal = (23, 7)
iconXY Icon'OrientationNormal = (24, 7)
iconXY Icon'OrientationView = (25, 7)
iconXY Icon'Copydown = (0, 6)
iconXY Icon'Pastedown = (1, 6)
iconXY Icon'Pasteflipup = (2, 6)
iconXY Icon'Pasteflipdown = (3, 6)
iconXY Icon'VisSel11 = (4, 6)
iconXY Icon'VisSel10 = (5, 6)
iconXY Icon'VisSel01 = (6, 6)
iconXY Icon'VisSel00 = (7, 6)
iconXY Icon'9_G = (8, 6)
iconXY Icon'AutomergeOff = (9, 6)
iconXY Icon'AutomergeOn = (10, 6)
iconXY Icon'12_G = (11, 6)
iconXY Icon'UvVertexsel = (12, 6)
iconXY Icon'UvEdgesel = (13, 6)
iconXY Icon'UvFacesel = (14, 6)
iconXY Icon'UvIslandsel = (15, 6)
iconXY Icon'UvSyncSelect = (16, 6)
iconXY Icon'18_G = (17, 6)
iconXY Icon'19_G = (18, 6)
iconXY Icon'20_G = (19, 6)
iconXY Icon'TransformOrigins = (20, 6)
iconXY Icon'Gizmo = (21, 6)
iconXY Icon'OrientationCursor = (22, 6)
iconXY Icon'NormalsVertex = (23, 6)
iconXY Icon'NormalsFace = (24, 6)
iconXY Icon'NormalsVertexFace = (25, 6)
iconXY Icon'ShadingBbox = (0, 5)
iconXY Icon'ShadingWire = (1, 5)
iconXY Icon'ShadingSolid = (2, 5)
iconXY Icon'ShadingRendered = (3, 5)
iconXY Icon'ShadingTexture = (4, 5)
iconXY Icon'Overlay = (5, 5)
iconXY Icon'Xray = (6, 5)
iconXY Icon'8_F = (7, 5)
iconXY Icon'9_F = (8, 5)
iconXY Icon'LockviewOff = (9, 5)
iconXY Icon'LockviewOn = (10, 5)
iconXY Icon'12_F = (11, 5)
iconXY Icon'AxisSide = (12, 5)
iconXY Icon'AxisFront = (13, 5)
iconXY Icon'AxisTop = (14, 5)
iconXY Icon'16_F = (15, 5)
iconXY Icon'17_F = (16, 5)
iconXY Icon'18_F = (17, 5)
iconXY Icon'19_F = (18, 5)
iconXY Icon'LayerUsed = (19, 5)
iconXY Icon'LayerActive = (20, 5)
iconXY Icon'22_F = (21, 5)
iconXY Icon'23_F = (22, 5)
iconXY Icon'24_F = (23, 5)
iconXY Icon'25_F = (24, 5)
iconXY Icon'26_F = (25, 5)
iconXY Icon'1_E = (0, 4)
iconXY Icon'2_E = (1, 4)
iconXY Icon'3_E = (2, 4)
iconXY Icon'4_E = (3, 4)
iconXY Icon'5_E = (4, 4)
iconXY Icon'6_E = (5, 4)
iconXY Icon'7_E = (6, 4)
iconXY Icon'8_E = (7, 4)
iconXY Icon'9_E = (8, 4)
iconXY Icon'10_E = (9, 4)
iconXY Icon'11_E = (10, 4)
iconXY Icon'12_E = (11, 4)
iconXY Icon'13_E = (12, 4)
iconXY Icon'14_E = (13, 4)
iconXY Icon'15_E = (14, 4)
iconXY Icon'16_E = (15, 4)
iconXY Icon'17_E = (16, 4)
iconXY Icon'18_E = (17, 4)
iconXY Icon'19_E = (18, 4)
iconXY Icon'20_E = (19, 4)
iconXY Icon'21_E = (20, 4)
iconXY Icon'22_E = (21, 4)
iconXY Icon'23_E = (22, 4)
iconXY Icon'Home = (23, 4)
iconXY Icon'Documents = (24, 4)
iconXY Icon'Temp = (25, 4)
iconXY Icon'Sortalpha = (0, 3)
iconXY Icon'Sortbyext = (1, 3)
iconXY Icon'Sorttime = (2, 3)
iconXY Icon'Sortsize = (3, 3)
iconXY Icon'Shortdisplay = (4, 3)
iconXY Icon'Longdisplay = (5, 3)
iconXY Icon'7_D = (6, 3)
iconXY Icon'Imgdisplay = (7, 3)
iconXY Icon'9_D = (8, 3)
iconXY Icon'10_D = (9, 3)
iconXY Icon'Bookmarks = (10, 3)
iconXY Icon'Fontpreview = (11, 3)
iconXY Icon'Filter = (12, 3)
iconXY Icon'Newfolder = (13, 3)
iconXY Icon'FolderRedirect = (14, 3)
iconXY Icon'FileParent = (15, 3)
iconXY Icon'FileRefresh = (16, 3)
iconXY Icon'FileFolder = (17, 3)
iconXY Icon'FileBlank = (18, 3)
iconXY Icon'FileBlend = (19, 3)
iconXY Icon'FileImage = (20, 3)
iconXY Icon'FileMovie = (21, 3)
iconXY Icon'FileScript = (22, 3)
iconXY Icon'FileSound = (23, 3)
iconXY Icon'FileFont = (24, 3)
iconXY Icon'FileText = (25, 3)
iconXY Icon'SortDesc = (0, 2)
iconXY Icon'SortAsc = (1, 2)
iconXY Icon'LinkBlend = (2, 2)
iconXY Icon'AppendBlend = (3, 2)
iconXY Icon'Import = (4, 2)
iconXY Icon'Export = (5, 2)
iconXY Icon'7_C = (6, 2)
iconXY Icon'8_C = (7, 2)
iconXY Icon'9_C = (8, 2)
iconXY Icon'10_C = (9, 2)
iconXY Icon'11_C = (10, 2)
iconXY Icon'12_C = (11, 2)
iconXY Icon'13_C = (12, 2)
iconXY Icon'LoopBack = (13, 2)
iconXY Icon'LoopForwards = (14, 2)
iconXY Icon'Back = (15, 2)
iconXY Icon'Forward = (16, 2)
iconXY Icon'18_C = (17, 2)
iconXY Icon'19_C = (18, 2)
iconXY Icon'FileArchive = (19, 2)
iconXY Icon'FileCache = (20, 2)
iconXY Icon'FileVolume = (21, 2)
iconXY Icon'File3d = (22, 2)
iconXY Icon'FileHidden = (23, 2)
iconXY Icon'FileBackup = (24, 2)
iconXY Icon'DiskDrive = (25, 2)
iconXY Icon'Matplane = (0, 1)
iconXY Icon'Matsphere = (1, 1)
iconXY Icon'Matcube = (2, 1)
iconXY Icon'Monkey = (3, 1)
iconXY Icon'Hair = (4, 1)
iconXY Icon'Aliased = (5, 1)
iconXY Icon'Antialiased = (6, 1)
iconXY Icon'MatSphereSky = (7, 1)
iconXY Icon'Matshaderball = (8, 1)
iconXY Icon'Matcloth = (9, 1)
iconXY Icon'Matfluid = (10, 1)
iconXY Icon'12_B = (11, 1)
iconXY Icon'WordwrapOff = (12, 1)
iconXY Icon'WordwrapOn = (13, 1)
iconXY Icon'SyntaxOff = (14, 1)
iconXY Icon'SyntaxOn = (15, 1)
iconXY Icon'LinenumbersOff = (16, 1)
iconXY Icon'LinenumbersOn = (17, 1)
iconXY Icon'Scriptplugins = (18, 1)
iconXY Icon'20_B = (19, 1)
iconXY Icon'21_B = (20, 1)
iconXY Icon'22_B = (21, 1)
iconXY Icon'Disc = (22, 1)
iconXY Icon'Desktop = (23, 1)
iconXY Icon'ExternalDrive = (24, 1)
iconXY Icon'NetworkDrive = (25, 1)
iconXY Icon'SeqSequencer = (0, 0)
iconXY Icon'SeqPreview = (1, 0)
iconXY Icon'SeqLumaWaveform = (2, 0)
iconXY Icon'SeqChromaScope = (3, 0)
iconXY Icon'SeqHistogram = (4, 0)
iconXY Icon'SeqSplitview = (5, 0)
iconXY Icon'SeqStripMeta = (6, 0)
iconXY Icon'SeqStripDuplicate = (7, 0)
iconXY Icon'9_A = (8, 0)
iconXY Icon'ImageRgb = (9, 0)
iconXY Icon'ImageRgbAlpha = (10, 0)
iconXY Icon'ImageAlpha = (11, 0)
iconXY Icon'ImageZdepth = (12, 0)
iconXY Icon'HandleAutoclamped = (13, 0)
iconXY Icon'HandleAuto = (14, 0)
iconXY Icon'HandleAligned = (15, 0)
iconXY Icon'HandleVector = (16, 0)
iconXY Icon'HandleFree = (17, 0)
iconXY Icon'19_A = (18, 0)
iconXY Icon'20_A = (19, 0)
iconXY Icon'21_A = (20, 0)
iconXY Icon'ViewPerspective = (21, 0)
iconXY Icon'ViewOrtho = (22, 0)
iconXY Icon'ViewCamera = (23, 0)
iconXY Icon'ViewPan = (24, 0)
iconXY Icon'ViewZoom = (25, 0)
