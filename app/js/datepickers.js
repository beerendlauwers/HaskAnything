function loadDatepickers() {
  jQuery('#dates-series').datepicker({
      multidate: true
  });
}

jQuery(document).ready( loadDatepickers );
