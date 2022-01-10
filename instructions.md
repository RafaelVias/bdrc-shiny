## How to use the app?

This Shiny application can be used to generate a discharge rating curve
from paired observations of stage and discharge. To start the data must
be uploaded.

<br />

## 1. Upload data

To upload data, select **Browse** in the Controls section

<img src="browse_pic.png" id="id" class="class" style="width:40.0%;height:40.0%" />

The uploaded file must be an excel file with at least two columns named
**W** and **Q**, containing the stage and discharge observations,
respectively. The stage observations must be given in **centimeters**
(*c**m*), and the the discharge in **cubic meters per second**
(*m*<sup>3</sup>/*s*). A test dataset can be downloaded by pushing
**Download xlxs test file** above the Browse button. The first row
contains the column names, including **W** and **Q**, and the
observations are in the corresponding columns. When uploaded, the
application then automatically selects only these two columns from the
file.

<img src="excel_sheet.png" id="id" class="class" style="width:60.0%;height:60.0%" />

<br />

## 2. Specify a Rating Curve Model

Once the data is uploaded, the Rating Curve Model can be specified.
There are two model characteristics that can be altered. First, the
**Rating Curve Type** can be set to either **Generalized Power-Law** or
**Power-law**. This setting specifies the type of rating curve the mean
of the model is to follow. In simple terms; the Generalized Power-law
rating curve is more flexible and is able to give a convincing fit to a
greater number of datasets. For this reason it is selected as the
default type. For more details on these rating curve differences go to
**Background** in the left sidebar menu. The second model specification
is to choose between a **Constant** or **Stage varying** residual
variance. If set to *stage varying* then the model tries to capture any
changes in the residual variance that happens over the stage values.
This can improve the predictive power of the model, and therefore is
selected as the default setting.

<img src="rc_spec.png" id="id" class="class" style="width:32.0%;height:32.0%" />

<br />

## 3. Run The Model!

Now that the model has been specified, the Rating Curve can be generated
by pressing the **Create Rating Curve** button.

<img src="create_rc.png" id="id" class="class" style="width:30.0%;height:30.0%" />

Once the model has finished running, the results will be presented as
Figures, Tables and Convergence Diagnostics Plots, which can all be
accessed from the tab panel in the **Rating Curve Builder**.

<br />
