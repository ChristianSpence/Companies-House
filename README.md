# Companies House
Tools for working with company data

- [Clean-reg-no.R](https://github.com/FutureEconomiesAnalytics/Companies-House/blob/main/Clean-reg-no.R) provides R source code and a regex parser to clean-up the most common problems in user-entered registered numbers and then validate against Companies House standards that the number *could* be valid (i.e. it does not actually check that it is a current/historic number). Validation rules from [Companies House Uniform Resource Identifiers (URI) Customer Guide](https://assets.publishing.service.gov.uk/government/uploads/system/uploads/attachment_data/file/809682/uniformResourceIdentifiersCustomerGuide.pdf).
- [BasicCompanyData - CREATE TABLE.sql](https://github.com/ChristianSpence/Companies-House/blob/main/BasicCompanyData-CREATE%20TABLE.sql) provides SQL Server commands to create a table according to the Companies House schema
