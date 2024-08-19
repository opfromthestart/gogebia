Gogebia

A spreadsheet editor build in Rust and SDL2 that is efficient for range based operations.

Usage:

This works just like Excel in simple cases:
![gog_1](https://github.com/user-attachments/assets/e685cec0-d72f-402c-8596-bcc5e21ed054)

Many familiar formulas are here as well:
![gog_2](https://github.com/user-attachments/assets/2391d6f7-2a08-4aef-a812-b079a91c1779)
![gog3](https://github.com/user-attachments/assets/e96a6dd4-edf0-4c49-b4f1-1c8fb9698db3)

One major change is how cells handle errors. Instead of being cryptic, each error will try to point to the cell that caused the error.
![gog4](https://github.com/user-attachments/assets/3f17ca55-6184-458a-b68e-73e40d96bba0)
![gogo4 5](https://github.com/user-attachments/assets/801f985a-0140-474e-a7ca-f28fe988832b)

Another change is that all typed cell references are static, so A1 means the same as $A$1 in Excel. In order to do relative positions, use the `value` function.
There are special values called `.C` and `.R` that hold the column and row numbers of the current cell.
![gog5](https://github.com/user-attachments/assets/5c2b6fac-62d8-47bb-989a-3d8b7c777b31)
(This one would break if copied/moved)
![gog6](https://github.com/user-attachments/assets/4441587f-9ebc-4621-9c32-df45a502285d)
(This one would not)

Another special value is `.F.`, which is used in some functions to specify a value in a given range.
This example counts how many numbers in the list are above 4.
![gog7](https://github.com/user-attachments/assets/4d3a0475-2a0f-417a-8e65-e5472282ea6f)

The big feature of this editor is range functions. This does the equivalent of "dragging a formula" but is self contained. It uses the syntax `[C,R]=F` to indicate that the formula takes up C+1 columns, R+1 rows, and is filled by the formula F.

Here is an example of it being used to double the value of the cells of column A.
![gog8](https://github.com/user-attachments/assets/ef5a3d96-1bef-440b-8b87-0f73f565e9ef)
To avoid the errors in later cells, you can specify if a cell should only be filled in certain cases with `[C,R,COND]=F`, where the cell will only be filled if `COND` is truthy (it is false if the cell is empty, an error, or false).
![gog8 5](https://github.com/user-attachments/assets/588e5a4f-c594-4a82-b0fe-ceec5c1b85c6)

These conditions can be any formula. Here is an example that prints a geometric sequence until it reaches 2:
![gog9](https://github.com/user-attachments/assets/1473f183-2364-46ba-a5e1-134f792fa8e1)
Once B1 is updated, the cells take on their new values, including cells that were previously empty.
![gog10](https://github.com/user-attachments/assets/fb4c9fef-0911-495b-b99d-f9e7e02e8587)

Here is an example of an amortization table that has all parameters, including number of periods:
![gog11](https://github.com/user-attachments/assets/8dd9a7b9-eddc-4c1c-a86d-e5e927a3fc24)
When the number of periods changes, so does the number of rows.
![gog12](https://github.com/user-attachments/assets/7a64d56d-22bb-43de-be7d-415d499e9103)


show filter with .F.
Show that range formulas cant intersect.
show multi file linking, new sheets made will be placed inside folder of name of original file.
