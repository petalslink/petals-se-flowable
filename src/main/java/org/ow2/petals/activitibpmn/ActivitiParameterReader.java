/**
 * Copyright (c) 2015-2016 Linagora
 * 
 * This program/library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2.1 of the License, or (at your
 * option) any later version.
 * 
 * This program/library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License
 * for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program/library; If not, see <http://www.gnu.org/licenses/>
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.activitibpmn;

import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_IDENTITY_SERVICE_CFG_FILE;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DEFAULT_ENGINE_IDENTITY_SERVICE_CLASS_NAME;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_DRIVER;

import java.io.File;
import java.util.logging.Logger;

import javax.jbi.JBIException;

import org.ow2.petals.activitibpmn.identity.IdentityService;

/**
 * Utility class to get the right value of component parameter. If the value of a paameter is invalid, a default value
 * can be returned
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class ActivitiParameterReader {

    private ActivitiParameterReader() {
        // NOP: Utility class -> No constructor
    }

    public static final String getJdbcDriver(final String jdbcDriverConfigured, final Logger log) {

        // TODO: Add a check to verify that the driver class exists

        if (jdbcDriverConfigured == null || jdbcDriverConfigured.trim().isEmpty()) {
            log.info("No JDBC Driver configured for database. Default value used.");
            return DEFAULT_JDBC_DRIVER;
        } else {
            return jdbcDriverConfigured;
        }
    }

    /**
     * 
     * @param identityServiceClassNameConfigured
     * @param log
     * @return
     * @throws JBIException
     *             The default identity service class name is not found
     */
    public static final Class<?> getEngineIdentityServiceClassName(final String identityServiceClassNameConfigured,
            final Logger log) throws JBIException {
        Class<?> identityServiceClass;
        try {
            if (identityServiceClassNameConfigured == null || identityServiceClassNameConfigured.trim().isEmpty()) {
                log.info("No identity service configured. Default value used.");
                return ActivitiSE.class.getClassLoader().loadClass(DEFAULT_ENGINE_IDENTITY_SERVICE_CLASS_NAME);
            } else {
                try {
                    identityServiceClass = ActivitiSE.class.getClassLoader().loadClass(
                            identityServiceClassNameConfigured.trim());
                    if (!IdentityService.class.isAssignableFrom(identityServiceClass)) {
                        log.warning("Identity service does not implement " + IdentityService.class.getName()
                                + ". Default value used.");
                        return ActivitiSE.class.getClassLoader().loadClass(DEFAULT_ENGINE_IDENTITY_SERVICE_CLASS_NAME);
                    } else {
                        return identityServiceClass;
                    }
                } catch (final ClassNotFoundException e) {
                    log.warning("Identity service class not found: " + identityServiceClassNameConfigured
                            + ". Default value used.");
                    return ActivitiSE.class.getClassLoader().loadClass(DEFAULT_ENGINE_IDENTITY_SERVICE_CLASS_NAME);
                }
            }
        } catch (final ClassNotFoundException e) {
            throw new JBIException("Default identity service not found.", e);
        }
    }

    public static final File getEngineIdentityServiceConfigurationFile(final String identityServiceCfgFileConfigured,
            final Logger log) {
        if (identityServiceCfgFileConfigured == null || identityServiceCfgFileConfigured.trim().isEmpty()) {
            log.info("No identity service configuration file configured. Default configuration used.");
            return DEFAULT_ENGINE_IDENTITY_SERVICE_CFG_FILE;
        } else {
            final File tmpFile = new File(identityServiceCfgFileConfigured.trim());
            if (!tmpFile.isAbsolute()) {
                log.warning("The identity service configuration file configured (" + identityServiceCfgFileConfigured
                        + ") is not an absolute file. Default configuration used.");
                return DEFAULT_ENGINE_IDENTITY_SERVICE_CFG_FILE;
            } else if (!tmpFile.exists()) {
                log.warning("The identity service configuration file configured (" + identityServiceCfgFileConfigured
                        + ") does not exist. Default configuration used.");
                return DEFAULT_ENGINE_IDENTITY_SERVICE_CFG_FILE;
            } else if (!tmpFile.isFile()) {
                log.warning("The identity service configuration file configured (" + identityServiceCfgFileConfigured
                        + ") is not a file. Default configuration used.");
                return DEFAULT_ENGINE_IDENTITY_SERVICE_CFG_FILE;
            } else {
                return tmpFile;
            }
        }
    }
}
