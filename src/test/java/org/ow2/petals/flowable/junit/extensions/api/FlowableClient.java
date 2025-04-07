/**
 * Copyright (c) 2024-2025 Linagora
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
package org.ow2.petals.flowable.junit.extensions.api;

import java.io.File;
import java.io.IOException;
import java.util.List;

import org.flowable.engine.HistoryService;
import org.flowable.engine.IdentityService;
import org.flowable.engine.ManagementService;
import org.flowable.engine.ProcessEngine;
import org.flowable.engine.RepositoryService;
import org.flowable.engine.RuntimeService;
import org.flowable.engine.TaskService;
import org.flowable.engine.impl.bpmn.parser.factory.XMLImporterFactory;
import org.flowable.engine.impl.cfg.ProcessEngineConfigurationImpl;
import org.flowable.engine.repository.Deployment;
import org.flowable.task.api.Task;
import org.ow2.petals.flowable.junit.extensions.FlowableClientExtension;

/**
 * The API of {@link FlowableClient} used as JUnit 5 extension allowing creation of a Flowable client.
 * 
 * @see FlowableClientExtension
 */
public interface FlowableClient {

    /**
     * The default JDBC URL pattern for an embedded in-memory H2 database
     */
    public static final String DEFAULT_H2_INMEMORY_JDBC_URL_PATTERN = "jdbc:h2:mem:%s;DB_CLOSE_DELAY=-1";

    /**
     * The default JDBC URL pattern for an embedded in-memory H2 database
     */
    public static final String DEFAULT_H2_FILE_JDBC_URL_PATTERN = "jdbc:h2:%s";

    /**
     * The default JDBC username
     */
    public static final String DEFAULT_JDBC_USERNAME = "sa";

    /**
     * The default JDBC username
     */
    public static final String DEFAULT_JDBC_PWD = "";

    /**
     * The default configuration file name to use the default configuration of the selected IDM engine
     */
    public static final String DEFAULT_IDM_ENGINE_CONFIGURATION = "DEFAULT_IDM_ENGINE_CONFIGURATION";

    /**
     * The name of the IDM engine configuration file used to generate a temporary IDM engine configuration
     */
    public static final String IDM_ENGINE_CONFIGURATION_TO_GENERATE = "IDM_ENGINE_CONFIGURATION_TO_GENERATE";

    /**
     * Initialize the Flowable client
     */
    public void init() throws Exception;

    /**
     * Set the configuration file of IDM Engine configurator.
     * 
     * @param idmEngineConfiguratorCfgFile
     *            The configuration file of IDM Engine configurator. If {@code null}, no IDM engine will be used. If the
     *            filename equals to {@value FlowableClient#IDM_ENGINE_CONFIGURATION_TO_GENERATE}, an empty tempory
     *            configuration file is generated, and can be retrieved with {@link #getIdmEngineConfigurationFile()}.
     * @throws IOException
     */
    public void setIdmEngineConfiguratorCfgFile(final File idmEngineConfiguratorCfgFile) throws IOException;

    /**
     * Start the Flowable client (ie. starts the embedded Flowable engines)
     */
    public void start() throws Exception;

    /**
     * @return The JDBC URL of the database used
     */
    public String getJdbcUrl();

    /**
     * @return The IDM engine configuration file
     */
    public File getIdmEngineConfigurationFile();

    /**
     * @return A reference to the Flowable process engine
     */
    public ProcessEngine getProcessEngine();

    /**
     * @return The {@link RuntimeService} of the Flowable engine
     */
    public default RuntimeService getRuntimeService() {
        return this.getProcessEngine().getRuntimeService();
    }

    /**
     * @return The {@link TaskService} of the Flowable engine
     */
    public default TaskService getTaskService() {
        return this.getProcessEngine().getTaskService();
    }

    /**
     * @return The {@link HistoryService} of the Flowable engine
     */
    public default HistoryService getHistoryService() {
        return this.getProcessEngine().getHistoryService();
    }

    /**
     * @return The {@link IdentityService} of the Flowable engine
     */
    public default org.flowable.engine.IdentityService getIdentityService() {
        return this.getProcessEngine().getIdentityService();
    }

    /**
     * @return The {@link ManagementService} of the Flowable engine
     */
    public default ManagementService getManagementService() {
        return this.getProcessEngine().getManagementService();
    }

    /**
     * @return The {@link RepositoryService} of the Flowable engine
     */
    public default RepositoryService getRepositoryService() {
        return this.getProcessEngine().getRepositoryService();
    }

    public default void setXMLImporterFactory(final XMLImporterFactory xmlImporterFactory) {
        ((ProcessEngineConfigurationImpl) this.getProcessEngine().getProcessEngineConfiguration())
                .setWsdlImporterFactory(xmlImporterFactory);
    }

    /**
     * Complete a user task
     * 
     * @param processInstanceId
     *            The identifier of process instance containing the user task to complete
     * @param userTaskDefKey
     *            The identifier of the user task to complete
     * @param candidateUser
     *            The user that completes the user task
     */
    public default void completeUserTask(final String processInstanceId, final String userTaskDefKey,
            final String candidateUser) {
        final Task userTask = this.getTaskService().createTaskQuery().processInstanceId(processInstanceId)
                .taskDefinitionKey(userTaskDefKey).taskCandidateUser(candidateUser).singleResult();

        // Before to complete the task, we set explicitly its assignee. It is not done by Flowable engine when
        // completing the task.
        this.getTaskService().setAssignee(userTask.getId(), candidateUser);

        // Now we can complete the user task
        this.getIdentityService().setAuthenticatedUserId(candidateUser);
        try {
            this.getTaskService().complete(userTask.getId());
        } finally {
            this.getIdentityService().setAuthenticatedUserId(null);
        }
    }

    /**
     * Purge Flowable database removing:
     * <ul>
     * <li>process instances,</li>
     * <li>history process instances,</li>
     * <li>process definitions,</li>
     * <li>deployments.</li>
     * </ul>
     */
    public default void purge() {
        final List<Deployment> deployments = this.getRepositoryService().createDeploymentQuery().list();
        for (final Deployment deployment : deployments) {
            this.getRepositoryService().deleteDeployment(deployment.getId(), true);
        }
    }
}
