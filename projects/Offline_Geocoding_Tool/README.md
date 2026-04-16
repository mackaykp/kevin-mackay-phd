# Offline Geocoding Tool

This project geocodes Canadian addresses without relying on online geocoding services.

It uses Statistics Canada National Address Register data stored in DuckDB, then matches input addresses from CSV or Excel files to likely geographic coordinates.

The tool is built for batch processing and can handle imperfect address text by combining exact, relaxed, and fuzzy matching steps.

Core scripts are written in R, with outputs saved as geocoded CSV files for downstream mapping or analysis.
