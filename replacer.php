<?php
// Replacer

$working_dir = '/Users/jwsm/Desktop/WACM/project/pdf_output/tmp';
$template_file = $working_dir . '/template.ly';
$output_file = $working_dir . '/analysis.ly';

$soprano_file = $working_dir . '/soprano.txt';
$bass_file = $working_dir . '/bass.txt';
$key_file = $working_dir . '/key.txt';
$chord_file = $working_dir . '/chords.txt';

$soprano_info = file_get_contents($soprano_file);
$bass_info = file_get_contents($bass_file);
$key_info = file_get_contents($key_file);
$chord_info = file_get_contents($chord_file);

$template_ly = file_get_contents($template_file);
$template_ly = preg_replace ('/##SOPRANO##/', $soprano_info, $template_ly);
$template_ly = preg_replace ('/##BASS##/', $bass_info, $template_ly);
$template_ly = preg_replace ('/##KEY##/', strtoupper($key_info), $template_ly); // assuming major for now
$template_ly = preg_replace ('/##KEY_LC##/', strtolower($key_info), $template_ly);
$template_ly = preg_replace ('/##CHORDS##/', $chord_info, $template_ly);
file_put_contents($output_file, $template_ly);

?>