# Form Designer component for Delphi (VCL)

![Demo](https://github.com/havlicekp/form-designer/blob/master/images/demo.gif)

Form Designer can be used to design and modify Delphi (VCL) forms at runtime. The behaviour and appearance is very similar to Form Designer available in Delphi IDE. 

The repository contains both the TFormDesigner component and Demo project.

## Installation

1. Download or fork the repository and open grouped project ``src\FormDesigner.groupproj``. 
2. Right-click on the FormDesigner.Package.dproj and choose **Install**
3. **Form Designer** tab will appear on your Component Toolbar 

## Usage

1. Drop the the **TFormDesigner** component onto a form
2. Customize component's self-explanatory properties like DragHandleColor, DrawGrid or SnapToGrid
3. During runtime call ``AddControl`` methods to instruct TFormDesigner which controls should be possible to move/re-size. There are two versions of ``AddControl``. 
* The first one works with controls already present on a form:
'''pascal
frmDesignForm.fdDesigner.AddControl(btnButton1);
'''
* The second version accepts class name (TClass) of a control to be created dynamically. The control is created when mouse is clicked on a form or mouse is dragged specifying new control dimensions.
'''pascal
frmDesignForm.fdDesigner.AddControl(TButton);
'''

