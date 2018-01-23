/**
 * Copyright (c) 2015-2018 Linagora
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
 * along with this program/library; If not, see http://www.gnu.org/licenses/
 * for the GNU Lesser General Public License version 2.1.
 */
package org.ow2.petals.flowable;

import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_JDBC_DRIVER;

import java.io.File;
import java.util.logging.Logger;

import javax.jbi.JBIException;

import org.ow2.petals.flowable.identity.SeFlowableIdmServiceConfigurator;

/**
 * Utility class to get the right value of component parameter. If the value of a parameter is invalid, a default value
 * can be returned
 * 
 * @author Christophe DENEUX - Linagora
 *
 */
public class FlowableParameterReader {

    private FlowableParameterReader() {
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
     * @param idmEngineConfiguratorClassNameConfigured
     * @param log
     * @return
     * @throws JBIException
     *             The default identity service class name is not found
     */
    public static final Class<?> getIdmEngineConfiguratorClassName(final String idmEngineConfiguratorClassNameConfigured,
            final Logger log) throws JBIException {
        final Class<?> idmEngineConfiguratorClass;
        try {
            if (idmEngineConfiguratorClassNameConfigured == null || idmEngineConfiguratorClassNameConfigured.trim().isEmpty()) {
                log.info("No IDM engine configurator configured. Default value used.");
                return FlowableSE.class.getClassLoader().loadClass(DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
            } else {
                try {
                    idmEngineConfiguratorClass = FlowableSE.class.getClassLoader().loadClass(
                            idmEngineConfiguratorClassNameConfigured.trim());
                    if (!SeFlowableIdmServiceConfigurator.class.isAssignableFrom(idmEngineConfiguratorClass)) {
                        log.warning("IDM engine configurator does not implement "
                                + SeFlowableIdmServiceConfigurator.class.getName()
                                + ". Default value used.");
                        return FlowableSE.class.getClassLoader().loadClass(DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
                    } else {
                        return idmEngineConfiguratorClass;
                    }
                } catch (final ClassNotFoundException e) {
                    log.warning("IDM engine configurator class not found: " + idmEngineConfiguratorClassNameConfigured
                            + ". Default value used.");
                    return FlowableSE.class.getClassLoader().loadClass(DEFAULT_IDM_ENGINE_CONFIGURATOR_CLASS_NAME);
                }
            }
        } catch (final ClassNotFoundException e) {
            throw new JBIException("Default IDM engine configurator not found.", e);
        }
    }

    public static final File getEngineIdentityServiceConfigurationFile(final String idmEngineConfiguratorCfgFileConfigured,
            final Logger log) {
        if (idmEngineConfiguratorCfgFileConfigured == null || idmEngineConfiguratorCfgFileConfigured.trim().isEmpty()) {
            log.info("No IDM engine configurator configuration file configured. Default configuration used.");
            return DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE;
        } else {
            final File tmpFile = new File(idmEngineConfiguratorCfgFileConfigured.trim());
            if (!tmpFile.isAbsolute()) {
                log.warning(String.format("The IDM engine configurator configuration file configured (%s) is not an absolute file. Default configuration used.", idmEngineConfiguratorCfgFileConfigured));
                return DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE;
            } else if (!tmpFile.exists()) {
                log.warning(String.format(
                        "The IDM engine configurator configuration file configured (%s) does not exist. Default configuration used.",
                        idmEngineConfiguratorCfgFileConfigured));
                return DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE;
            } else if (!tmpFile.isFile()) {
                log.warning(String.format(
                        "The IDM engine configurator configuration file configured (%s) is not a file. Default configuration used.",
                        idmEngineConfiguratorCfgFileConfigured));
                return DEFAULT_IDM_ENGINE_CONFIGURATOR_CFG_FILE;
            } else {
                return tmpFile;
            }
        }
    }
}
