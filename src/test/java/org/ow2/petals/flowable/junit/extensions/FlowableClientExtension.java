/**
 * Copyright (c) 2024 Linagora
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
package org.ow2.petals.flowable.junit.extensions;

import static org.apiguardian.api.API.Status.STABLE;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

import org.apiguardian.api.API;
import org.h2.Driver;
import org.junit.jupiter.api.extension.ExtendWith;
import org.ow2.petals.flowable.junit.extensions.api.FlowableClient;
import org.ow2.petals.flowable.junit.extensions.impl.FlowableClientExtensionImpl;

/**
 * <p>
 * The annotation to use in unit tests to use a Flowable client.
 * </p>
 * <p>
 * This JUnit 5 extension allows creation of a Flowable client that is guaranteed to be deleted when the test method
 * finishes (whether it passes or fails):
 * </p>
 * 
 * <pre>
 * public class MyTestSuite {
 *  &#064;FlowableClientExtension
 *  private FlowableClient flowableClient;
 * 
 *  &#064;Test
 *  public void test...() throws IOException {
 *      final ProcessInstance processInstance = flowableClient.getProcessInstance(processInstanceId);
 *      ...
 *  }
 * }
 * </pre>
 */
@Target({ ElementType.FIELD, ElementType.PARAMETER })
@Retention(RetentionPolicy.RUNTIME)
@ExtendWith({ FlowableClientExtensionImpl.class })
@API(status = STABLE, since = "1.5.0")
public @interface FlowableClientExtension {

    public enum DatabaseType {
        // To use for embedded in-memory database
        IN_MEMORY,
        // To use for file-based database
        FILE_BASED
    }

    public enum IdmEngineType {
        // To use the file-based IDM engine
        FILE_BASED,
        // To use the LDAP IDM engine
        LDAP
    }

    /**
     * <p>
     * The JDBC Driver of the database to use.
     * </p>
     * <p>
     * <b>Default value</b>: {@link Driver}
     * </p>
     */
    @API(status = STABLE, since = "1.5.0")
    public Class<? extends java.sql.Driver> jdbcDriver() default Driver.class;

    /**
     * <p>
     * The database type.
     * </p>
     * <p>
     * <b>Default value</b>: {@link DatabaseType#IN_MEMORY}.
     * </p>
     */
    @API(status = STABLE, since = "1.5.0")
    public DatabaseType databaseType() default DatabaseType.IN_MEMORY;

    /**
     * <p>
     * The JDBC URL pattern of the database to use.
     * </p>
     * <p>
     * The pattern accept only one argument that will be fill with the name of the embedded in-memory database, or the
     * file path of the file-based database. Two pre-defined patterns exist:
     * {@code FlowableClient#DEFAULT_H2_INMEMORY_JDBC_URL_PATTERN} and
     * {@code FlowableClient#DEFAULT_H2_FILE_JDBC_URL_PATTERN}
     * </p>
     * <p>
     * <b>Default value</b>: The JDBC URL pattern of an embedded in-memory H2 database:
     * {@value FlowableClient#DEFAULT_H2_INMEMORY_JDBC_URL_PATTERN}
     * </p>
     */
    @API(status = STABLE, since = "1.5.0")
    public String jdbcUrlPattern() default FlowableClient.DEFAULT_H2_INMEMORY_JDBC_URL_PATTERN;

    /**
     * <p>
     * The JDBC username of the database to use.
     * </p>
     * <p>
     * <b>Default value</b>: {@value FlowableClient#DEFAULT_JDBC_USERNAME}
     * </p>
     */
    @API(status = STABLE, since = "1.5.0")
    public String jdbcUser() default FlowableClient.DEFAULT_JDBC_USERNAME;

    /**
     * <p>
     * The JDBC username password of the database to use.
     * </p>
     * <p>
     * <b>Default value</b>: {@value FlowableClient#DEFAULT_JDBC_PWD}
     * </p>
     */
    @API(status = STABLE, since = "1.5.0")
    public String jdbcPassword() default FlowableClient.DEFAULT_JDBC_PWD;

    /**
     * <p>
     * The IDM engine type.
     * </p>
     * <p>
     * <b>Default value</b>: {@link IdmEngineType#FILE_BASED}.
     * </p>
     */
    @API(status = STABLE, since = "1.5.0")
    public IdmEngineType idmEngineType() default IdmEngineType.FILE_BASED;

    /**
     * <p>
     * The configuration file of the IDM engine configurator, as resource name or file name.
     * </p>
     * <p>
     * If the IDM engine type is {@link IdmEngineType#FILE_BASED}:
     * </p>
     * <ul>
     * <li>the file given here must be a valid configuration file for the file-based IDM engine,</li>
     * <li>the value {@value FlowableClient#IDM_ENGINE_CONFIGURATION_TO_GENERATE} will generate an empty configuration
     * of the file-based IDM engine without users, groups and privileges defined.</li>
     * <li>the configuration provided here can be completes with {@link #idmEngineAddUsers()},
     * {@link #idmEngineAddGroups()} and {@link #idmEngineAddPrivileges()}.</li>
     * </ul>
     * <p>
     * If the IDM engine type is {@link IdmEngineType#LDAP}:
     * </p>
     * <ul>
     * <li>the file given here must be a valid configuration file for the LDAP IDM engine.</li>
     * </ul>
     * <p>
     * <b>Default value</b>: {@value FlowableClient#DEFAULT_IDM_ENGINE_CONFIGURATION} to use the default configuration
     * of the selected IDM engine
     * </p>
     */
    @API(status = STABLE, since = "1.5.0")
    public String idmEngineConfiguratorCfgFile() default FlowableClient.DEFAULT_IDM_ENGINE_CONFIGURATION;

    /**
     * Add the following user definitions to the IDM engine.
     */
    @API(status = STABLE, since = "1.5.0")
    public IdmUser[] idmEngineAddUsers() default {};

    /**
     * Add the following group definitions to the IDM engine.
     */
    @API(status = STABLE, since = "1.5.0")
    public IdmGroup[] idmEngineAddGroups() default {};

    /**
     * Add the following privilege definitions to the IDM engine.
     */
    @API(status = STABLE, since = "1.5.0")
    public IdmPrivilege[] idmEngineAddPrivileges() default {};
    
    /**
     * If {@code true}, this Flowable client will be automatically started. If {@code false}, extra configuration could be done before to explicitly starts this Flowable Client with {@link FlowableClient#start()}.
     */
    @API(status = STABLE, since = "1.5.0")
    public boolean autoStart() default true;
}
