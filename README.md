# Form Designer component for Delphi (VCL)

![Demo](https://github.com/havlicekp/form-designer/blob/master/images/demo.gif)

Form Designer (TFormDesigner) can be used to design and modify Delphi (VCL) forms at runtime. The behavior and appearance is  similar to that of Delphi IDE. 

The repository contains both the TFormDesigner component and Demo project.

## Installation

1. Download or fork the repository and open grouped project ``src\FormDesigner.groupproj``. 
2. Right-click on the FormDesigner.Package.dproj and choose **Install**
3. **Form Designer** tab will appear on your Component Toolbar 

## Usage

1. Drop the the **TFormDesigner** component onto a form
2. Customize component's self-explanatory properties like DragHandleColor, DrawGrid or SnapToGrid
3. During runtime call ``AddControl`` methods to instruct TFormDesigner which controls should be possible to move/re-size. There are two versions of ``AddControl``. 
  
   * The first accepts an instance of an existing control - typically, this would be a control already present on a TForm.
   ```pascal
   procedure AddControl(AControl: TControl); overload;
   ```
  
   * The second version accepts class of a control (TControlClass) to be created dynamically. The control will be created by user action, either by a mouse click on a form hosting the TFormDesigner or by dragging a mouse around thus specifying new control dimensions.
   ```pascal
   procedure TFormDesigner.AddControl(ControlClass: TControlClass); overload 
   ```
4. Set ``Enabled`` to False to take away TFormDesigner's reign over the hosting TForm and its controls.

## Notes 

* TFormDesigner was tested only under **Windows 10** and Embarcadero® **Delphi 10.3** Community Edition
* The code is peppered with more recent language features like **generics**, **anonymous methods** or **class helpers**. Although, tt should be fairly easy to revert those and use the code in older version of Delphi.
