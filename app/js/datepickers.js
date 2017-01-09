function loadDatepickers() {
  jQuery('#dates-series').datepicker({
      multidate: true,
      format: "yyyy-mm-dd"
  });

  jQuery('#date-published').datepicker({
    format: "yyyy-mm-dd"
  });
}

jQuery(document).ready( loadDatepickers );
