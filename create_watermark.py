#!/usr/bin/env python3
"""
Forensic watermark + metadata + encryption script
Compatible with pypdf 6.1.1
"""

from reportlab.pdfgen import canvas
from reportlab.lib.pagesizes import letter
from pypdf import PdfReader, PdfWriter
from datetime import datetime
import uuid
import os
import sys

# --------- Settings ----------
input_file = "summary_report.pdf"
output_file = "report_forensic_protected.pdf"
watermark_file = "watermark_temp.pdf"

recipient = "dawid.kucharski"

user_password = "r7!FqZ9#bV2pX@4uK8mT6wL"
owner_password = "^T9vG#3kLm8!pZ2qR7sW4xY6uF0bN5@h"

rotation_deg = 60
font_size_main = 36
font_size_footer = 10
light_gray = (0.85, 0.85, 0.85)

# ------------------------------------------
# Check input file exists
if not os.path.isfile(input_file):
    print(f"ERROR: input file not found: {input_file}")
    sys.exit(1)

# Generate forensic ID and timestamp
forensic_id = str(uuid.uuid4())[:13]
timestamp = datetime.utcnow().strftime("%Y-%m-%d %H:%M UTC")

watermark_lines = [
    "DRAFT – FOR INTERNAL USE ONLY",
    f"Recipient: {recipient}",
    f"ID: {forensic_id}"
]

footer_text = f"{recipient} | ID: {forensic_id} | {timestamp}"

# ---------------------------
# Step 1: create watermark PDF
# ---------------------------
c = canvas.Canvas(watermark_file, pagesize=letter)
width, height = letter

c.setFont("Helvetica-Bold", font_size_main)
try:
    c.setFillAlpha(0.8)
except Exception:
    pass
c.setFillColorRGB(*light_gray)

c.saveState()
c.translate(width/2, height/2)
c.rotate(rotation_deg)

for i, line in enumerate(watermark_lines):
    y_off = 30 - i * (font_size_main + 6)
    c.drawCentredString(0, y_off, line)

c.restoreState()

# Footer
c.setFont("Helvetica", font_size_footer)
try:
    c.setFillAlpha(0.8)
except Exception:
    pass
c.setFillColorRGB(0.55, 0.55, 0.55)
footer_y = 18
c.drawCentredString(width/2, footer_y, footer_text)

# Tiny corners
try:
    c.setFillAlpha(0.08)
except Exception:
    pass
c.setFont("Helvetica", 6)
c.setFillColorRGB(0.7, 0.7, 0.7)
c.drawString(10, 10, f"{recipient} | {forensic_id}")
c.drawRightString(width-10, 10, f"{timestamp}")

c.save()
print("Watermark page generated:", watermark_file)

# ---------------------------
# Step 2: apply watermark to each page
# ---------------------------
reader = PdfReader(input_file)
watermark_page = PdfReader(watermark_file).pages[0]
writer = PdfWriter()

for page in reader.pages:
    page.merge_page(watermark_page)
    writer.add_page(page)

# ---------------------------
# Step 3: metadata
# ---------------------------
metadata = {
    "/Title": os.path.basename(output_file),
    "/Author": "YourOrganizationName",
    "/Subject": "Confidential - Internal",
    "/Keywords": f"forensic_id:{forensic_id},recipient:{recipient}",
    "/Creator": "forensic_watermark_script",
    "/Producer": "reportlab+pypdf"
}
writer.add_metadata(metadata)

# ---------------------------
# Step 4: encrypt PDF (pypdf 6.1.1) with permissions_flag
# ---------------------------
# Bitwise value for permissions (0 = deny everything, 4 = allow printing, etc.)
# To deny printing, copying, modifying, use 0
permissions_flag = 0

writer.encrypt(
    user_password=user_password,
    owner_password=owner_password,
    use_128bit=True,
    permissions_flag=permissions_flag
)

# ---------------------------
# Save PDF
# ---------------------------
with open(output_file, "wb") as f_out:
    writer.write(f_out)

# Remove temporary watermark
try:
    os.remove(watermark_file)
except OSError:
    pass

print("Done.")
print(f"Output file: {output_file}")
print(f"Forensic ID: {forensic_id}")
print(f"Recipient: {recipient}")
print(f"Timestamp: {timestamp}")
