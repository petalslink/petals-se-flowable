/**
 * Copyright (c) 2018-2020 Linagora
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
package org.ow2.petals.flowable.utils;

import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
import static org.ow2.petals.flowable.FlowableSEConstants.DEFAULT_ENGINE_REST_API_ENABLE;
import static org.ow2.petals.flowable.FlowableSEConstants.DBServer.DEFAULT_DATABASE_SCHEMA_UPDATE;

import java.util.logging.Logger;

public class ConfigurationValueParser {

    private ConfigurationValueParser() {
        // Utility class --> No constructor
    }

    /**
     * Parse a value associated to the parameter driving the database schema update. Possible values: {false, true,
     * create-drop }.
     * 
     * @param databaseSchemaUpdateString
     *            The value to parse
     * @param logger
     *            A logger to return message
     * @return The parsed value
     */
    public static String parseDatabaseSchemaUpdate(final String databaseSchemaUpdateString, final Logger logger) {

        if (databaseSchemaUpdateString == null || databaseSchemaUpdateString.trim().isEmpty()) {
            logger.info("No schema update processing configured for database. Default value used.");
            return DEFAULT_DATABASE_SCHEMA_UPDATE;
        } else if ("false".equals(databaseSchemaUpdateString.trim()) || "true".equals(databaseSchemaUpdateString.trim())
                || "create-drop".equals(databaseSchemaUpdateString.trim())) {
            return databaseSchemaUpdateString.trim();
        } else {
            logger.info("Invalid value '" + databaseSchemaUpdateString
                    + "' configured for the schema update processing. Default value used.");
            return DEFAULT_DATABASE_SCHEMA_UPDATE;
        }
    }

    /**
     * Parse a value associated to the parameter enabling the asynchronous job executor.
     * 
     * @param enableFlowableJobExecutorConfigured
     *            The value to parse
     * @param logger
     *            A logger to return message
     * @return The parsed value
     */
    public static boolean parseEngineEnableJobExecutor(final String enableFlowableJobExecutorConfigured,
            final Logger logger) {

        // Caution:
        // - only the value "false", ignoring case and spaces will disable the job executor,
        // - only the value "true", ignoring case and spaces will enable the job executor,
        // - otherwise, the default value is used.
        if (enableFlowableJobExecutorConfigured == null || enableFlowableJobExecutorConfigured.trim().isEmpty()) {
            logger.info("The activation of the Flowable job executor is not configured. Default value used.");
            return DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR;
        } else {
            return "false".equalsIgnoreCase(enableFlowableJobExecutorConfigured.trim()) ? false
                    : ("true".equalsIgnoreCase(enableFlowableJobExecutorConfigured.trim()) ? true
                            : DEFAULT_ENGINE_ENABLE_JOB_EXECUTOR);
        }
    }

    /**
     * Parse a value associated to the parameter enabling the BPMN validation
     * 
     * @param enableFlowableBpmnValidationConfigured
     *            The value to parse
     * @param logger
     *            A logger to return message
     * @return The parsed value
     */
    public static boolean parseEngineEnableBpmnValidation(final String enableFlowableBpmnValidationConfigured,
            final Logger logger) {

        // Caution:
        // - only the value "false", ignoring case and spaces will disable the BPMN validation,
        // - only the value "true", ignoring case and spaces will enable the BPMN validation,
        // - otherwise, the default value is used.
        if (enableFlowableBpmnValidationConfigured == null || enableFlowableBpmnValidationConfigured.trim().isEmpty()) {
            logger.info(
                    "The activation of the BPMN validation on process deployments into Flowable engine is not configured. Default value used.");
            return DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION;
        } else {
            return "false".equalsIgnoreCase(enableFlowableBpmnValidationConfigured.trim()) ? false
                    : ("true".equalsIgnoreCase(enableFlowableBpmnValidationConfigured.trim()) ? true
                            : DEFAULT_ENGINE_ENABLE_BPMN_VALIDATION);
        }
    }

    /**
     * Parse a value associated to the parameter enabling the Flowable REST API
     * 
     * @param configuredEngineRestApiEnable
     *            The value to parse
     * @param logger
     *            A logger to return message
     * @return The parsed value
     */
    public static boolean parseEngineRestApiEnable(final String configuredEngineRestApiEnable, final Logger logger) {

        // Caution:
        // - only the value "false", ignoring case and spaces will disable the rest api,
        // - only the value "true", ignoring case and spaces will enable the rest api,
        // - otherwise, the default value is used.
        final boolean engineRestApiEnable;
        if (configuredEngineRestApiEnable == null || configuredEngineRestApiEnable.trim().isEmpty()) {
            logger.info("The activation of the Flowable REST API is not configured. Default value used.");
            engineRestApiEnable = DEFAULT_ENGINE_REST_API_ENABLE;
        } else {
            engineRestApiEnable = "false".equalsIgnoreCase(configuredEngineRestApiEnable.trim()) ? false
                    : ("true".equalsIgnoreCase(configuredEngineRestApiEnable.trim()) ? true
                            : DEFAULT_ENGINE_REST_API_ENABLE);
        }
        return engineRestApiEnable;
    }

}
