/**
 * Copyright (c) 2015 Linagora
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
package org.ow2.petals.activitibpmn.junit;

import static org.junit.Assert.fail;
import static org.ow2.petals.activitibpmn.ActivitiSEConstants.DBServer.DEFAULT_JDBC_DRIVER;

import java.io.File;
import java.net.MalformedURLException;

import org.activiti.engine.HistoryService;
import org.activiti.engine.IdentityService;
import org.activiti.engine.ProcessEngine;
import org.activiti.engine.ProcessEngineConfiguration;
import org.activiti.engine.RuntimeService;
import org.activiti.engine.TaskService;
import org.junit.Rule;
import org.junit.rules.ExternalResource;

/**
 * The {@link ActivitiClient} {@link Rule} allows creation of a client connected to an Activiti server. It is guaranteed
 * to be deleted when the test method finishes (whether it passes or fails):
 * 
 * <pre>
 * public class MyTestSuite {
 *  &#064;Rule
 *  public ActivitiClient activitiClient = new ActivitiClient(...);
 * 
 *  &#064;Test
 *  public void test...() throws IOException {
 *      ProcessInstance processInstance = activitiClient.getProcessInstance(processInstanceId);
 *      ...
 *  }
 * }
 * </pre>
 */
public class ActivitiClient extends ExternalResource {

    /**
     * The default JDBC username
     */
    public static final String DEFAULT_JDBC_USERNAME = "sa";

    /**
     * The default JDBC username
     */
    public static final String DEFAULT_JDBC_PWD = "";

    /**
     * The JDBC driver used to join the Activiti database
     */
    private final String jdbcDriver;

    /**
     * The JDBC Url locating the Activiti database
     */
    private final String jdbcUrl;

    /**
     * The JDBC username of the Activiti database
     */
    private final String jdbcUsername;

    /**
     * The JDBC password of the Activiti database
     */
    private final String jdbcPwd;

    private ProcessEngine activitiClientEngine;

    /**
     * <p>
     * Creates a temporary Activiti client connected to a default Activiti server:
     * <ul>
     * <li>JDBC Driver: {@link #DEFAULT_JDBC_DRIVER},</li>
     * <li>JDBC URL: an embedded H2 database as file,</li>
     * <li>JDBC username: {@link #DEFAULT_JDBC_USERNAME},</li>
     * <li>JDBC password: {@link #DEFAULT_JDBC_PWD}.</li>
     * </ul>
     * </p>
     * 
     * @param fileForJdbcUrl
     *            The file path of the Activiti database set as JDBC URL
     */
    public ActivitiClient(final File fileForJdbcUrl) {
        this(DEFAULT_JDBC_DRIVER, String.format("jdbc:h2:%s", convertFile2Url(fileForJdbcUrl)), DEFAULT_JDBC_USERNAME,
                DEFAULT_JDBC_PWD);
    }

    private static String convertFile2Url(final File fileForJdbcUrl) {
        try {
            return fileForJdbcUrl.toURI().toURL().toExternalForm();
        } catch (final MalformedURLException e) {
            fail(e.getMessage());
            return null;
        }
    }

    public ActivitiClient(final String jdbcDriver, final String jdbcUrl, final String jdbcUsername, final String jdbcPwd) {
        this.jdbcDriver = jdbcDriver;
        this.jdbcUrl = jdbcUrl;
        this.jdbcUsername = jdbcUsername;
        this.jdbcPwd = jdbcPwd;
    }

    @Override
    protected void before() throws Throwable {
        this.initializeActivitiClient();
    }

    @Override
    protected void after() {
        try {
            this.destroyActivitiClient();
        } catch (final Exception e) {
            e.printStackTrace();
        }
    }

    /**
     * Initialize & start the Activiti client
     */
    private void initializeActivitiClient() throws Exception {

        final ProcessEngineConfiguration pec = ProcessEngineConfiguration.createStandaloneProcessEngineConfiguration();
        pec.setJdbcDriver(this.jdbcDriver);
        pec.setJdbcUrl(this.jdbcUrl);
        pec.setJdbcUsername(this.jdbcUsername).setJdbcPassword(this.jdbcPwd);
        pec.setDatabaseSchemaUpdate("true");

        this.activitiClientEngine = pec.buildProcessEngine();
    }

    /**
     * Free the Activiti client, without freeing the Activiti database
     */
    private void destroyActivitiClient() throws Exception {

        this.activitiClientEngine.close();
    }

    /**
     * @return The {@link RuntimeService} of the Activiti engine
     */
    public RuntimeService getRuntimeService() {
        return this.activitiClientEngine.getRuntimeService();
    }

    /**
     * @return The {@link TaskService} of the Activiti engine
     */
    public TaskService getTaskService() {
        return this.activitiClientEngine.getTaskService();
    }

    /**
     * @return The {@link HistoryService} of the Activiti engine
     */
    public HistoryService getHistoryService() {
        return this.activitiClientEngine.getHistoryService();
    }

    /**
     * @return The {@link IdentityService} of the Activiti engine
     */
    public IdentityService getIdentityService() {
        return this.activitiClientEngine.getIdentityService();
    }

}
