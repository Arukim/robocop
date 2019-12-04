robocopy aicup2019 submit/tmp *.fs* /s /xd obj /xd bin /xd .* /xf *.user /purge

7z a submit\submit-$(Get-Date -f yyyyMMdd.hhmmss).zip .\submit\tmp\*
