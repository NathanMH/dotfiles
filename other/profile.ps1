Set-Location C:\Users\natha\Documents

for($i = 1; $i -le 5; $i++){
    $u = "".PadLeft($i,"u")
    $unum = "u$i"
    $d = $u.Replace("u","../")
    Invoke-Expression "function $u { push-location $d }"
    Invoke-Expression "function $unum { push-location $d }"
}