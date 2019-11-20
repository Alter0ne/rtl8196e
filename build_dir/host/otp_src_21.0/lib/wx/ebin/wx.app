%% This is an -*- erlang -*- file.
%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2010-2016. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%

{application, wx,
 [{description, "Yet another graphics system"},
  {vsn, "1.8.4"},
  {modules,
   [
    %% Generated modules
  wxAcceleratorEntry, wxAcceleratorTable, wxActivateEvent, wxArtProvider, wxAuiDockArt, wxAuiManager, wxAuiManagerEvent, wxAuiNotebook, wxAuiNotebookEvent, wxAuiPaneInfo, wxAuiSimpleTabArt, wxAuiTabArt, wxBitmapButton, wxBitmapDataObject, wxBitmap, wxBoxSizer, wxBrush, wxBufferedDC, wxBufferedPaintDC, wxButton, wxCalendarCtrl, wxCalendarDateAttr, wxCalendarEvent, wxCaret, wxCheckBox, wxCheckListBox, wxChildFocusEvent, wxChoicebook, wxChoice, wxClientDC, wxClipboard, wxClipboardTextEvent, wxCloseEvent, wxColourData, wxColourDialog, wxColourPickerCtrl, wxColourPickerEvent, wxComboBox, wxCommandEvent, wxContextMenuEvent, wxControl, wxControlWithItems, wxCursor, wxDataObject, wxDateEvent, wxDatePickerCtrl, wxDC, wxDCOverlay, wxDialog, wxDirDialog, wxDirPickerCtrl, wxDisplayChangedEvent, wxDropFilesEvent, wxEraseEvent, wxEvent, wxEvtHandler, wxFileDataObject, wxFileDialog, wxFileDirPickerEvent, wxFilePickerCtrl, wxFindReplaceData, wxFindReplaceDialog, wxFlexGridSizer, wxFocusEvent, wxFontData, wxFontDialog, wxFont, wxFontPickerCtrl, wxFontPickerEvent, wxFrame, wxGauge, wxGBSizerItem, wxGenericDirCtrl, wxGLCanvas, wxGraphicsBrush, wxGraphicsContext, wxGraphicsFont, wxGraphicsMatrix, wxGraphicsObject, wxGraphicsPath, wxGraphicsPen, wxGraphicsRenderer, wxGridBagSizer, wxGridCellAttr, wxGridCellBoolEditor, wxGridCellBoolRenderer, wxGridCellChoiceEditor, wxGridCellEditor, wxGridCellFloatEditor, wxGridCellFloatRenderer, wxGridCellNumberEditor, wxGridCellNumberRenderer, wxGridCellRenderer, wxGridCellStringRenderer, wxGridCellTextEditor, wxGrid, wxGridEvent, wxGridSizer, wxHelpEvent, wxHtmlEasyPrinting, wxHtmlLinkEvent, wxHtmlWindow, wxIconBundle, wxIcon, wxIconizeEvent, wxIdleEvent, wxImage, wxImageList, wxInitDialogEvent, wxJoystickEvent, wxKeyEvent, wxLayoutAlgorithm, wxListbook, wxListBox, wxListCtrl, wxListEvent, wxListItemAttr, wxListItem, wxListView, wxLocale, wxLogNull, wxMask, wxMaximizeEvent, wxMDIChildFrame, wxMDIClientWindow, wxMDIParentFrame, wxMemoryDC, wxMenuBar, wxMenu, wxMenuEvent, wxMenuItem, wxMessageDialog, wxMiniFrame, wxMirrorDC, wx_misc, wxMouseCaptureChangedEvent, wxMouseCaptureLostEvent, wxMouseEvent, wxMoveEvent, wxMultiChoiceDialog, wxNavigationKeyEvent, wxNotebook, wxNotebookEvent, wxNotifyEvent, wxOverlay, wxPageSetupDialogData, wxPageSetupDialog, wxPaintDC, wxPaintEvent, wxPaletteChangedEvent, wxPalette, wxPanel, wxPasswordEntryDialog, wxPen, wxPickerBase, wxPopupTransientWindow, wxPopupWindow, wxPostScriptDC, wxPreviewCanvas, wxPreviewControlBar, wxPreviewFrame, wxPrintData, wxPrintDialogData, wxPrintDialog, wxPrinter, wxPrintout, wxPrintPreview, wxProgressDialog, wxQueryNewPaletteEvent, wxRadioBox, wxRadioButton, wxRegion, wxSashEvent, wxSashLayoutWindow, wxSashWindow, wxScreenDC, wxScrollBar, wxScrolledWindow, wxScrollEvent, wxScrollWinEvent, wxSetCursorEvent, wxShowEvent, wxSingleChoiceDialog, wxSizeEvent, wxSizer, wxSizerFlags, wxSizerItem, wxSlider, wxSpinButton, wxSpinCtrl, wxSpinEvent, wxSplashScreen, wxSplitterEvent, wxSplitterWindow, wxStaticBitmap, wxStaticBox, wxStaticBoxSizer, wxStaticLine, wxStaticText, wxStatusBar, wxStdDialogButtonSizer, wxStyledTextCtrl, wxStyledTextEvent, wxSysColourChangedEvent, wxSystemOptions, wxSystemSettings, wxTaskBarIcon, wxTaskBarIconEvent, wxTextAttr, wxTextCtrl, wxTextDataObject, wxTextEntryDialog, wxToggleButton, wxToolBar, wxToolbook, wxToolTip, wxTopLevelWindow, wxTreebook, wxTreeCtrl, wxTreeEvent, wxUpdateUIEvent, wxWindowCreateEvent, wxWindowDC, wxWindowDestroyEvent, wxWindow, wxXmlResource, glu, gl,
    %% Handcrafted modules
    wx,
    wx_object,
    wxe_master,
    wxe_server,
    wxe_util
   ]},
  {registered, []},
  {applications, [stdlib, kernel]},
  {env, []},
  {runtime_dependencies, ["stdlib-2.0","kernel-3.0","erts-6.0"]}
 ]}.
